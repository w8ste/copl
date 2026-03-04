(module

  ;; Define a table to hold function references (with one entry)
  (table 2 funcref)
  ;; Initialize the table with the $add (idx0) and $sub (idx1)
  (elem (i32.const 0) $add $sub)

  ;; Define the function type
  (type $funcHeader (func (param i32 i32) (result i32)))

  (func $add (param $a i32) (param $b i32) (result i32)
    (i32.add
      (local.get $a)
      (local.get $b)
    )
  )
  
  (func $sub (param $a i32) (param $b i32) (result i32)
    (i32.sub
      (local.get $a)
      (local.get $b)
    )
  )

  ;; Higher-order function: Get a function pointer from the table and apply it
  (func $apply_from_table (param $index i32) (param $a i32) (param $b i32) (result i32)
    ;; Use call_indirect to call the function in the table at the given index
    (call_indirect (type $funcHeader)
      (local.get $a) ;; First argument to the function
      (local.get $b) ;; Second argument to the function
      (local.get $index) ;; Index in the table
    )
  )

  ;; Export the higher-order function so it can be called externally
  (export "apply_function" (func $apply_from_table))
)
