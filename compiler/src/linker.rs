use std::error::Error;
use wasmtime::*;
use wasmtime_wasi::{Wasi, WasiCtx};

pub fn link(
    source_filename: &str,
    lib_filename: &str,
    lib_name: &str,
) -> Result<(), Box<dyn Error>> {
    let store = Store::default();

    // First set up our linker which is going to be linking modules together. We
    // want our linker to have wasi available, so we set that up here as well.
    let mut linker = Linker::new(&store);
    let wasi = Wasi::new(&store, WasiCtx::new(std::env::args())?);
    wasi.add_to_linker(&mut linker)?;

    // Load and compile our two modules
    let linking1 = Module::from_file(&store, source_filename)?;
    let linking2 = Module::from_file(&store, lib_filename)?;

    // Instantiate our first module which only uses WASI, then register that
    // instance with the linker since the next linking will use it.
    let linking2 = linker.instantiate(&linking2)?;
    linker.instance(lib_name, &linking2)?;

    // And with that we can perform the final link and the execute the module.
    let linking1 = linker.instantiate(&linking1)?;
    let run = linking1.get_func("_start").unwrap();
    let run = run.get0::<()>()?;
    run()?;
    Ok(())
}
