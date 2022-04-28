use bitflags::bitflags;

bitflags! {
    pub struct CFSR: u32 {
        //                   |                |        |  MMFSR |
        const IACCVIOL    = 0b0000000000000000_00000000_00000001;
        const DACCVIOL    = 0b0000000000000000_00000000_00000010;
        const MUNSTKERR   = 0b0000000000000000_00000000_00001000;
        const MSTKERR     = 0b0000000000000000_00000000_00010000;
        const MLSPERR     = 0b0000000000000000_00000000_00100000;
        const MMARVALID   = 0b0000000000000000_00000000_10000000;
        //                   |                |  BFSR  |        |
        const IBUSERR     = 0b0000000000000000_00000001_00000000;
        const PRECISERR   = 0b0000000000000000_00000010_00000000;
        const IMPRECISERR = 0b0000000000000000_00000100_00000000;
        const UNSTKERR    = 0b0000000000000000_00001000_00000000;
        const STKERR      = 0b0000000000000000_00010000_00000000;
        const LSPERR      = 0b0000000000000000_00100000_00000000;
        const BFARVALID   = 0b0000000000000000_10000000_00000000;
        //                   |     UFSR       |        |        |
        const UNDEFINSTR  = 0b0000000000000001_00000000_00000000;
        const INVSTATE    = 0b0000000000000010_00000000_00000000;
        const INVPC       = 0b0000000000000100_00000000_00000000;
        const NOCP        = 0b0000000000001000_00000000_00000000;
        const STKOF       = 0b0000000000010000_00000000_00000000;
        const UNALIGNED   = 0b0000000100000000_00000000_00000000;
        const DIVBYZERO   = 0b0000001000000000_00000000_00000000;
    }
}

impl CFSR {
    pub const ADDR: u32 = 0xe000ed28;
    pub fn decode_error(&self) -> Vec<&'static str> {
        let mut out = Vec::new();
        if self.contains(Self::IACCVIOL) {
            out.push("Instruction Access Violation");
        }
        if self.contains(Self::DACCVIOL) {
            out.push("Data Access Violation");
        }
        if self.contains(Self::MUNSTKERR) {
            out.push("Memory Manage fault during Unstacking");
        }
        if self.contains(Self::MSTKERR) {
            out.push("Memory Manage fault during Stacking");
        }
        if self.contains(Self::MLSPERR) {
            out.push("Memory Manage fault during Lazy FP State Preservation");
        }

        if self.contains(Self::IBUSERR) {
            out.push("Instruction Bus Error");
        }
        if self.contains(Self::PRECISERR) {
            out.push("Precise Data Bus Error");
        }
        if self.contains(Self::IMPRECISERR) {
            out.push("Imprecise Data Bus Error");
        }
        if self.contains(Self::UNSTKERR) {
            out.push("Bus fault during unstacking");
        }
        if self.contains(Self::STKERR) {
            out.push("Bus fault during exception entry stacking");
        }
        if self.contains(Self::LSPERR) {
            out.push("Bus fault during lazy floating point stacking");
        }

        if self.contains(Self::UNDEFINSTR) {
            out.push("Undefined Instruction");
        }
        if self.contains(Self::INVSTATE) {
            out.push("Invalid State");
        }
        if self.contains(Self::INVPC) {
            out.push("Invalid PC");
        }
        if self.contains(Self::NOCP) {
            out.push("No Coprcessor: Atempted to execute an FP instruction without FP support or with FP disabled");
        }
        if self.contains(Self::STKOF) {
            out.push("Stack Underflow");
        }
        if self.contains(Self::UNALIGNED) {
            out.push("Atempted Unaligned Accesss");
        }
        if self.contains(Self::DIVBYZERO) {
            out.push("Divide by Zero: integer division by zero");
        }
        out
    }
}

bitflags! {
    pub struct SFSR: u32 {
        //      RES0       |        |
        const INVEP     = 0b00000001;
        const INVIS     = 0b00000010;
        const INVVER    = 0b00000100;
        const AUVIOL    = 0b00001000;
        const INVTRAN   = 0b00010000;
        const LSPERR    = 0b00100000;
        const SFARVALID = 0b01000000;
        const LSERR     = 0b10000000;
    }
}

impl SFSR {
    pub const ADDR: u32 = 0xe000ede4;
    pub fn decode_error(&self) -> Vec<&'static str> {
        let mut out = Vec::new();
        if self.contains(Self::INVEP) {
            out.push("Invalid Entry Point: Invalid attempt to enter Secure state");
        }
        if self.contains(Self::INVIS) {
            out.push("Invalid Integrity Signature: Stack integrity signature corrupted");
        }
        if self.contains(Self::INVVER) {
            out.push("Invalid Exception Return: NS partition tried to return to secure partition");
        }
        if self.contains(Self::AUVIOL) {
            out.push("Attribution Unit Violation: NS attempted to access a secure-only addresss");
        }
        if self.contains(Self::INVTRAN) {
            out.push("Invalid Transition: An untagged branch tried to cross domains (S->NS or NS->S)");
        }
        if self.contains(Self::LSPERR) {
            out.push("Lazy Stack Preservation Error: Lazy Floating-point stacking caused an SAU r IDAU error");
        }
        if self.contains(Self::LSERR) {
            out.push("Lazy State Error: An error occured during lazy state activation or deactivation");
        }
        out
    }
}

pub struct SFAR(u32);

impl SFAR {
    pub const ADDR: u32 = 0xe000ede8;
}

pub struct BFAR(u32);

impl BFAR {
    pub const ADDR: u32 = 0xe000ed38;
}

pub struct MMFAR(u32);

impl MMFAR {
    pub const ADDR: u32 = 0xe000ed34;
}