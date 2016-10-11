: 2null   0 0 ;
: big-size   200 100 ;

create winning-personality
  4 cells , 19 , 0 , 0 ,
  ' noop , ' noop , ' noop ,
  'emit @ ,
  'type @ ,
  '?type @ ,
  'cr @ ,
  ' noop , \ page
  ' drop , \ attribute
  'key @ ,
  'key? @ ,
  'ekey @ ,
  'ekey? @ ,
  'akey @ ,
  'pushtext @ ,
  ' 2drop , \ at-xy
  ' 2null , \ get-xy
  ' big-size , \ get-size
  'accept @ ,

winning-personality open-personality
