(module
    (func $add (param $var1 i32) (param $var2 i32)
        (result i32)
        local.get $var1             ;;pushing $var1 onto the stack
        local.get $var1             ;;pushing $var2 onto the stack
        i32.add)                     ;;adding the last 2 elements on the stack and saving as return value
    (export "add" (func $add))      ;;we are exporting $add WebAssembly function, the name "add" is used for JS imports
)
