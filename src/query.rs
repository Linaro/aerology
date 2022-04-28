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
pub enum Filter {
    Expr(Postfix),
    LLNodes(Postfix),
    Backtrace(SourceSpan, Vec<RegAssignment>),
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
            _ => unreachable!(),
        }
    }

    pub fn span(&self) -> SourceSpan {
        match self {
            Self::Backtrace(span, ..)
            | Self::LLNodes(Postfix { span, .. })
            | Self::Expr(Postfix { span, .. }) => span.clone(),
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
    pub global: Global,
    pub filters: Vec<Filter>,
}

impl FromStr for Query {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        let pairs = QueryParser::parse(Rule::query, s)?;
        let mut global = None;
        let mut filters = Vec::new();
        for pair in pairs {
            match pair.as_rule() {
                Rule::global => global = Some(Global::from_pair(pair)),
                Rule::postfix => filters.push(Filter::from_pair(pair)),
                Rule::filter => filters.push(Filter::from_pair(pair.into_inner().next().unwrap())),
                _ => (),
            }
        }
        let global = global.unwrap();
        Ok(Self { global, filters })
    }
}
