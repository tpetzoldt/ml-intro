library(DiagrammeR)


grViz("digraph ANN {

  rankdir=LR
  splines=line

  node [fixedsize=true, label=''];

  subgraph cluster_0 {
    color=white;
    node [style=solid, shape=circle];
    x1 x2 x3;
    label = 'input layer';
  }

  subgraph cluster_1 {
    color=white;
    node [style=solid,shape=circle];
    a12 a22 a32 a42 a52;
    label = 'hidden layer';
  }

  subgraph cluster_2 {
    color=white;
    node [style=solid, shape=circle];
    o1 o2;
    label='output layer';
  }

  {x1 x2 x3} -> {a12 a22 a32 a42 a52} -> {o1 o2};

}")


#############################################

grViz("digraph neuron {

  rankdir=LR
  splines=line

  {
  node [style=solid, shape=circle, label='', width=.5];
    n;
  }
  node [shape=none, margin=0.05, width=0.0];
    x1 x2 x3 y;

  {x1 x2 x3} -> n -> y
}")


grViz("digraph neuron {

  rankdir=LR
  splines=line

  {
  node [style=solid, shape=circle, label='', width=.5];
    n;
  }
  node [shape=none, margin=0.05, width=0.0];
    x1 x2 x3 y;

  x1 -> n [label=<w<SUB>1,1</SUB>>]
  x2 -> n [label='w2']
  x3 -> n [label='w3']
  n  -> y [label='w4']
}")

