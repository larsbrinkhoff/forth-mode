\ allocator.fth --- Indirection layer for memory allocation.
\
\ The idea is that we can have data structures that work with or
\ without regions.

\ The layout is designed so that 2@ produces: env vtable
begin-structure /allocator
  field: allocator-vtable
  field: allocator-env
end-structure

begin-structure /allocator-vtable
  field: %allocator-alloc	\ xt: u env -- a-addr ior
  field: %allocator-free	\ xt: a-addr env -- ior
  field: %allocator-resize	\ xt: a-addr1 u env -- a-addr2 ior
end-structure

: init-allocator-vtable ( alloc free resize a-addr -- allocator-vtable )
  >r
  r@ %allocator-resize !
  r@ %allocator-free !
  r@ %allocator-alloc !
  r>
;

: %default-alloc ( u env -- a-addr ior ) drop allocate ;
: %default-free ( a-addr env -- a-addr ior ) drop free ;
: %default-resize ( a-addr u env -- a-addr2 ior ) drop resize ;

: %make-default-allocator-vtable ( -- vtable )
  ['] %default-alloc ['] %default-free ['] %default-resize
  /allocator allocate throw
  init-allocator-vtable
;

0 value %default-allocator-vtable

: default-allocator ( -- env vtable )
  0
  %default-allocator-vtable dup 0= if
    drop
    %make-default-allocator-vtable dup to %default-allocator-vtable
  then
;

\ The type "allocator" in a stack effect means a pair: env vtable.
: allocator-alloc ( u allocator -- a-addr ior ) %allocator-alloc @ execute ;
: allocator-free ( a-addr allocator -- ior )    %allocator-free  @ execute ;

: allocator-resize ( a-addr1 u allocator -- a-addr2 ior )
  %allocator-resize @ execute
;
