
    (module
	  (func $fsub1 (param i32) (result i32)
		(local i32)
		(i32.sub (local.get 0) (i32.const 1))
	  )

	  (func $fsub2 (param i32) (result i32)
		(local i32)
		(i32.sub (local.get 0) (i32.const 2))
	  )
	  (func $fadd5 (param i32) (result i32)
		(local i32)
		(i32.add (local.get 0) (i32.const 5))
	  )

	  ;; function gets one argument and calls
	  ;; calls functions fadd5, fsub2, fsub1 in that order
	  (func $fcombined (param i32) (result i32)
		(local i32)
		(local.set 0 (call $fsub1 (local.get 0)))
		(local.set 0 (call $fsub2 (local.get 0)))
		(local.set 0 (call $fadd5 (local.get 0)))
		local.get 0

	  )

      (func $main (param i32) (result i32)
	    (call $fcombined (local.get 0))
	
      )


      (export "main" (func $main))
  )
	
    
