/* { dg-do run } */

var x = "bbb";

/* { dg-command { x < "aaa" } } */
/* { dg-output "0" } */

/* For constant folding: */
/* { dg-command { "bbb" < "aaa" } } */
/* { dg-output "\n0" } */