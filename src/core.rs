use std::collections::BTreeMap;
use std::fs::File;
use std::path::PathBuf;
use std::io::Read;

use goblin::elf::Elf;

use thiserror::Error;
use miette::Diagnostic;

#[derive(Clone, Debug)]
struct CoreRegion{
    size: usize,
    offset: usize,
}

#[derive(Default)]
pub struct Core {
    bytes: Vec<u8>,
    regions: BTreeMap<u32, Vec<CoreRegion>>
}

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error(transparent)]
    GoblinError(#[from] goblin::error::Error),
    #[error("Read from an invalid Address {0:x}")]
    InvalidAddress(u32),
}

pub type Result<T> = std::result::Result<T, Error>;

impl TryFrom<PathBuf> for Core {
    type Error = Error;
    fn try_from(path: PathBuf) -> Result<Self> {
        let mut core = File::open(path)?;
        let mut bytes = Vec::new();
        core.read_to_end(&mut bytes)?;
        // Do some basic error checking...
        let mut core = Self{ bytes, ..Default::default() };
        let elf = Elf::parse(&core.bytes)?;
        for phdr in &elf.program_headers {
            let size = phdr.p_memsz as usize;
            let offset = phdr.p_offset as usize;
            core.regions.entry(phdr.p_vaddr as u32)
                .or_default()
                .push(CoreRegion{ size, offset });
        }
        Ok(core)
    }
}

impl Core {
    pub fn read_into(&self, address: u32, buff: &mut [u8]) -> Result<()> {
        for (region_base, regs) in self.regions.range(..=address).rev() {
            for CoreRegion{size, offset} in regs {
                let region_offset = (address - region_base) as usize;
                if *region_base > address {
                    return Err(Error::InvalidAddress(address))
                } else if region_offset + buff.len() > *size {
                    continue
                } else {
                    let file_offset = region_offset + offset;
                    let from = &self.bytes[file_offset..file_offset+buff.len()];
                    buff.copy_from_slice(from);
                    return Ok(())
                }
            }
        }
        Err(Error::InvalidAddress(address))
    }
    
    pub fn addr_present(&self, address: u32) -> bool {
        for (region_base, regs) in self.regions.range(..=address).rev() {
            for CoreRegion{size, ..} in regs {
                let region_offset = (address - region_base) as usize;
                if *region_base > address {
                    return false
                } else if region_offset > *size {
                    continue
                } else {
                    return true
                }
            }
        }
        false
    }
}
