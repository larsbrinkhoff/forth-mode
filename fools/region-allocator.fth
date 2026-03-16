\ region-allocator.fth --- Turn a region into an allocator

: %region-allocator-alloc ( u region -- a-addr ior ) region-alloc 0 ;

: %region-allocator-free ( a-addr region -- ior )
  true abort" can't call free on regions"
;

: %region-allocator-resize ( a-addr1 u region -- a-addr2 ior )
  true abort" resize nyi"
;

: %make-region-allocator-vtable ( -- vtable )
  ['] %region-allocator-alloc
  ['] %region-allocator-free
  ['] %region-allocator-resize
  /allocator-vtable allocate throw init-allocator-vtable
;

0 value %region-allocator-vtable

: region>allocator ( region -- allocator )
  %region-allocator-vtable dup 0= if
    drop
    %make-region-allocator-vtable dup to %region-allocator-vtable
  then
;
