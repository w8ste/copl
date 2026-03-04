(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    (local i32 i32 i32 i32)
    i32.const 0
    local.set 1
    i32.const 1
    local.set 2
    loop  ;; label = @1
      local.get 1
      local.set 3
      local.get 2
      local.set 1
      local.get 3
      local.get 2
      i32.add
      local.set 2
      local.get 1
      local.get 0
      i32.le_s
      br_if 0 (;@1;)
    end
    local.get 3)
  (memory (;0;) 1)
  (export "memory" (memory 0))
  (export "fibonacci" (func 0)))
