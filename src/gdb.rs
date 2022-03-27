use std::io::{Read, Write};
use std::net::{Ipv4Addr, SocketAddr, SocketAddrV4, TcpStream};
use std::time::Duration;

use miette::Diagnostic;
use thiserror::Error;

const GDB_PACKET_START: char = '$';
const GDB_PACKET_END: char = '#';
const GDB_PACKET_ACK: char = '+';
#[allow(unused)]
const GDB_PACKET_HALT: u8 = 3;

pub struct Client {
    stream: TcpStream,
}

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error(transparent)]
    Utf8Error(#[from] std::str::Utf8Error),
    #[error(transparent)]
    TryFromIntError(#[from] std::num::TryFromIntError),
    #[error(transparent)]
    ParseIntError(#[from] std::num::ParseIntError),
    #[error("Missing start of GDB packet {0:?}")]
    MissingStart(String),
    #[error("End before Start in GDB packet {0:?}")]
    EndBeforeStart(String),
    #[error("Missing ACK after GDB packet {0:?}")]
    NoAck(String),
    #[error("Extra ACK found before GDB packet {0:?}")]
    ExtraAck(String),
    #[error("Incorrectly sized packet {0:?}")]
    IncorrectSize(String),
    #[error("Returned error code {0}")]
    ErrorCode(String),
}

type Result<T> = std::result::Result<T, Error>;

impl Client {
    pub fn new(port: u16) -> Result<Self> {
        let addr = SocketAddr::V4(SocketAddrV4::new(Ipv4Addr::new(127, 0, 0, 1), port));
        let timeout = Duration::from_millis(100);
        let stream = TcpStream::connect_timeout(&addr, timeout)?;
        let mut client = Self { stream };
        client.sendcmd("qSupported")?;
        // We need to tell qemu that we're a real gdb and we understand how
        // the g command works
        client.sendcmd("qXfer:features:read:target.xml:0,1000")?;
        client.sendcmd("qXfer:features:read:arm-m-profile.xml:0,1000")?;
        Ok(client)
    }

    fn preparecmd(&self, cmd: &str) -> Vec<u8> {
        let mut payload = vec![GDB_PACKET_START as u8];

        let mut chksum = 0;

        for &b in cmd.as_bytes() {
            payload.push(b);
            chksum += b as u32;
        }
        let ending = format!("{}{:02x}", GDB_PACKET_END, chksum & 0xFF);
        payload.extend_from_slice(ending.as_bytes());
        payload
    }

    fn sendcmd(&mut self, cmd: &str) -> Result<String> {
        let payload = self.preparecmd(cmd);
        self.stream.write_all(&payload)?;
        self.recieve(true)
    }

    fn ack(&mut self) -> Result<()> {
        Ok(self.stream.write_all(&[GDB_PACKET_ACK as u8])?)
    }

    fn recieve(&mut self, want_ack: bool) -> Result<String> {
        let mut buff = vec![0; 1024];
        let mut result = String::new();

        loop {
            let num = self.stream.read(&mut buff)?;
            result.push_str(std::str::from_utf8(&buff[..num])?);
            // The end delimeter is followed by a 2-byte checksum
            if result.len() >= 3 && result.find(GDB_PACKET_END) >= Some(result.len() - 3) {
                break;
            }
        }
        self.ack()?;
        let start = result
            .find(GDB_PACKET_START)
            .ok_or_else(|| Error::MissingStart(result.clone()))?;
        // If the loop above terminated, there's an end char
        let end = result.find(GDB_PACKET_END).unwrap();
        if end < start {
            Err(Error::EndBeforeStart(result.clone()))?
        }

        match result.find(GDB_PACKET_ACK) {
            Some(ack_pos) if want_ack && ack_pos > start => Err(Error::NoAck(result)),
            Some(ack_pos) if !want_ack && ack_pos < start => Err(Error::ExtraAck(result)),
            None if want_ack => Err(Error::NoAck(result)),
            _ => {
                let result = result[start + 1..end].to_string();
                if result.starts_with("E") {
                    Err(Error::ErrorCode(result[1..].to_string()))
                } else {
                    Ok(result)
                }
            }
        }
    }

    #[allow(unused)]
    fn send_only_ack(&mut self, cmd: &str) -> Result<()> {
        let mut buff = vec![0; 1];
        let to_send = self.preparecmd(cmd);
        self.stream.write_all(&to_send)?;
        let amount = self.stream.read(&mut buff)?;
        if amount != 1 {
            Err(Error::IncorrectSize(
                String::from_utf8_lossy(&buff).into_owned(),
            ))?
        }
        if buff[0] != GDB_PACKET_ACK as u8 {
            Err(Error::NoAck(String::from_utf8_lossy(&buff).into_owned()))?
        }
        Ok(())
    }

    pub fn read(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        const MAX_CHUNK_SIZE: u32 = 0x800;
        for (i, chunk) in data.chunks_mut(MAX_CHUNK_SIZE as usize).enumerate() {
            let cmd = format!(
                "m{:x},{:x}",
                addr + (i as u32 * MAX_CHUNK_SIZE),
                chunk.len()
            );
            let ret = self.sendcmd(&cmd)?;
            if ret.len() != chunk.len() * 2 {
                Err(Error::IncorrectSize(ret.clone()))?
            }
            for i in (0..ret.len()).step_by(2) {
                chunk[i / 2] = u8::from_str_radix(&ret[i..=i + 1], 16)?;
            }
        }
        Ok(())
    }

    pub fn read_regs(&mut self) -> Result<Vec<u32>> {
        let ret = self.sendcmd("g")?;
        let mut out = Vec::new();
        for j in (0..ret.len()).step_by(8) {
            let mut bytes = [0u8; 4];
            let chunk = &ret[j..=j+7];
            for i in (0..chunk.len()).step_by(2) {
                bytes[i / 2] = u8::from_str_radix(&chunk[i..=i + 1], 16)?;
            }
            out.push(u32::from_le_bytes(bytes))
        }
        Ok(out)
    }

    #[allow(unused)]
    pub fn halt(&mut self) -> Result<()> {
        self.stream.write_all(&[GDB_PACKET_HALT])?;
        self.recieve(false)?;
        Ok(())
    }

    #[allow(unused)]
    pub fn run(&mut self) -> Result<()> {
        self.send_only_ack("c")
    }
}
