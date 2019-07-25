204
((3) 0 () 1 ((q lib "meta-engine/main.rkt")) () (h ! (equal) ((c form c (c (? . 0) q define-component)) q (141 . 2)) ((c def c (c (? . 0) q entity)) q (64 . 3)) ((c def c (c (? . 0) q game)) q (0 . 3))))
procedure
(game entity ...) -> game?
  entity : entity?
procedure
(entity component ...) -> entity?
  component : component?
syntax
(define-component name contract)
procedure
(add-component entity component) -> entity?
  entity : entity?
  component : component?
procedure
(get-component entity query?) -> (or/c #f component?)
  entity : entity?
  query? : (or/c component? (-> component? boolean?))
procedure
(update-component entity old new) -> entity?
  entity : entity?
  old : (or/c component? (-> component? boolean?))
  new : (or/c component? (-> component? component?))
procedure
(remove-component entity old) -> entity?
  entity : entity?
  old : (or/c component? (-> component? boolean?))
procedure
(add-entity game new) -> game?
  game : game?
  new : entity?
procedure
(get-entity game query) -> entity?
  game : game?
  query : (or/c entity? (-> entity? boolean?))
procedure
(update-entity game old new) -> game?
  game : game?
  old : (or/c entity? (-> entity? boolean?))
  new : (or/c entity? (-> entity? component?))
procedure
(remove-entity game old) -> game?
  game : game?
  old : (or/c entity? (-> entity? boolean?))
procedure
(has-component query) -> (-> entity? boolean?)
  query : (-> component? boolean?)
procedure
(tick game) -> game?
  game : game?
procedure
(tick-entity entity) -> entity?
  entity : entity?
procedure
(tick-component component) -> component?
  component : component?
procedure
(ticks n game) -> game?
  n : number?
  game : game?
procedure
(tick-list game n) -> (listof game?)
  game : game?
  n : positive?
