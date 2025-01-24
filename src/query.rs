use std::str::FromStr;

use pest::Parser;
use pest_derive::Parser;

use miette::SourceSpan;

use crate::error::{Error, Result};

#[derive(Parser)]
#[grammar = "query.pest"]
struct QueryParser;

type Pair<'a> = pest::iterators::Pair<'a, Rule>;

pub fn span(pair: &Pair) -> SourceSpan {
    let span = pair.as_span();
    (span.start(), span.end() - span.start()).into()
}

#[derive(Debug)]
pub struct Symbol {
    pub name: String,
    pub span: SourceSpan,
}

impl Symbol {
    fn from_pair(pair: &Pair) -> Self {
        Self {
            name: pair.as_str().to_string(),
            span: span(pair),
        }
    }
}

#[derive(Debug)]
pub struct Global {
    pub elf: Option<Symbol>,
    pub sym: Symbol,
    pub span: SourceSpan,
}

impl Global {
    fn from_pair(pair: Pair) -> Self {
        let span = span(&pair);
        match &pair.into_inner().collect::<Vec<_>>()[..] {
            [elf, sym] => Self {
                elf: Some(Symbol::from_pair(elf)),
                sym: Symbol::from_pair(sym),
                span,
            },
            [sym] => Self {
                elf: None,
                sym: Symbol::from_pair(sym),
                span,
            },
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct GlobalCast {
    pub typ: Symbol,
    pub span: SourceSpan,
}

impl GlobalCast {
    fn from_pair(pair: Pair) -> Self {
        let span = span(&pair);
        let pair = pair.into_inner().next().unwrap();
        assert!(pair.as_rule() == Rule::typename);
        Self { typ: Symbol::from_pair(&pair), span }
    }
}

#[derive(Debug)]
pub enum Suffix {
    Index(Option<u32>, SourceSpan),
    Member(Symbol),
    Deref(SourceSpan),
}

impl Suffix {
    fn from_pair(p: Pair) -> Self {
        match p.as_rule() {
            Rule::index => {
                let span = span(&p);
                let inner = p.into_inner().next().and_then(|i| i.as_str().parse().ok());
                Self::Index(inner, span)
            }
            Rule::member => Self::Member(Symbol::from_pair(&p.into_inner().next().unwrap())),
            Rule::deref => Self::Deref(span(&p)),
            _ => unreachable!(),
        }
    }

    pub fn span(&self) -> SourceSpan {
        match self {
            Self::Member(Symbol { span, .. }) | Self::Index(_, span) | Self::Deref(span) => {
                span.clone()
            }
        }
    }
}

#[derive(Debug)]
pub struct Postfix {
    pub suffixes: Vec<Suffix>,
    pub span: SourceSpan,
}

impl Postfix {
    fn from_pair(p: Pair) -> Self {
        let span = span(&p);
        let suffixes = p.into_inner().map(Suffix::from_pair).collect();
        Self { suffixes, span }
    }
}

#[derive(Debug)]
pub struct RegAssignment {
    pub reg: Symbol,
    pub val: Postfix,
}

impl RegAssignment {
    fn from_pair(p: Pair) -> Self {
        let mut reg = None;
        let mut val = None;
        for pair in p.into_inner() {
            match pair.as_rule() {
                Rule::symbol => {
                    reg = Some(Symbol::from_pair(&pair));
                }
                Rule::postfix => {
                    val = Some(Postfix::from_pair(pair));
                }
                _ => (),
            }
        }
        let reg = reg.unwrap();
        let val = val.unwrap();
        Self { reg, val }
    }
}

#[derive(Debug)]
pub struct Cast {
    pub typ: Symbol,
    pub pf: Option<Postfix>,
}

impl Cast {
    fn from_pair(p: Pair) -> Self {
        let mut typ = None;
        let mut pf = None;
        for pair in p.into_inner() {
            match pair.as_rule() {
                Rule::typecast => typ = Some(Symbol::from_pair(&pair.into_inner().next().unwrap())),
                Rule::postfix => pf = Some(Postfix::from_pair(pair)),
                _ => unreachable!(),
            }
        }
        let typ = typ.unwrap();
        Self { typ, pf }
    }
}
#[derive(Debug)]
pub enum First {
    Cast(GlobalCast),
    Global(Global),
}

impl First {
    fn from_pair(p: Pair) -> Self {
        let inner = p.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::global => Self::Global(Global::from_pair(inner)),
            Rule::typecast => Self::Cast(GlobalCast::from_pair(inner)),
            _ => unreachable!(),
        }
    }

    pub fn span(&self) -> &SourceSpan {
        match self {
            Self::Cast(c) => &c.span,
            Self::Global(g) => &g.span,
        }
    }

    pub fn name(&self) -> &String {
        match self {
            Self::Cast(c) => &c.typ.name,
            Self::Global(g) => &g.sym.name,
        }
    }
}


#[derive(Debug)]
pub enum Filter {
    Expr(Postfix),
    LLNodes(Postfix),
    Backtrace(SourceSpan, Vec<RegAssignment>),
    Cast(Cast),
}

impl Filter {
    fn from_pair(p: Pair) -> Self {
        match p.as_rule() {
            Rule::postfix => Self::Expr(Postfix::from_pair(p)),
            Rule::backtrace => Self::Backtrace(
                span(&p),
                p.into_inner().map(RegAssignment::from_pair).collect(),
            ),
            Rule::llnodes => Self::LLNodes(Postfix::from_pair(p)),
            Rule::cast => Self::Cast(Cast::from_pair(p)),
            _ => unreachable!(),
        }
    }

    pub fn span(&self) -> SourceSpan {
        match self {
            Self::Backtrace(span, ..)
            | Self::LLNodes(Postfix { span, .. })
            | Self::Expr(Postfix { span, .. }) => span.clone(),
            Self::Cast(Cast {
                typ: Symbol { span, .. },
                ..
            }) => span.clone(),
        }
    }
}

impl FromStr for Filter {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        let pairs = QueryParser::parse(Rule::filter, s)?;
        let mut filter = None;
        for pair in pairs {
            for p in pair.into_inner() {
                filter = Some(Self::from_pair(p));
            }
        }
        let filter = filter.unwrap();
        Ok(filter)
    }
}

#[derive(Debug)]
pub struct Query {
    pub first: First,
    pub filters: Vec<Filter>,
}

impl FromStr for Query {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        let pairs = QueryParser::parse(Rule::query, s)?;
        let mut first = None;
        let mut filters = Vec::new();
        for pair in pairs {
            match pair.as_rule() {
                Rule::first => first = Some(First::from_pair(pair)),
                Rule::postfix => filters.push(Filter::from_pair(pair)),
                Rule::filter => filters.push(Filter::from_pair(pair.into_inner().next().unwrap())),
                _ => (),
            }
        }
        let first= first.unwrap();
        Ok(Self { first, filters })
    }
}
