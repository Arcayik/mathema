use std::{collections::HashMap, sync::{LazyLock, Mutex}};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
struct SymbolId(u32);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Symbol(SymbolId);

impl Symbol {
    pub fn intern(s: &str) -> Self {
        let mut table = SYMBOL_TABLE.lock().unwrap();

        if let Some(&id) = table.map.get(s) {
            return Symbol(id);
        }

        let leaked: &'static str = Box::leak(s.to_owned().into_boxed_str());
        let id = SymbolId(table.strings.len() as u32);
        table.strings.push(leaked);
        table.map.insert(leaked, id);

        Symbol(id)
    }

    pub fn as_str(&self) -> &'static str {
        let table = SYMBOL_TABLE.lock().unwrap();
        &table.strings[self.0.0 as usize]
    }
}


struct StringTable {
    map: HashMap<&'static str, SymbolId>,
    strings: Vec<&'static str>
}

pub static SYMBOL_TABLE: LazyLock<Mutex<StringTable>> = LazyLock::new(||
    Mutex::new(StringTable {
        map: HashMap::new(),
        strings: Vec::new()
    })
);
