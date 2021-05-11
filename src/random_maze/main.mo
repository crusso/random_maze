import Random "mo:base/Random";
import Array "mo:base/Array";
import List "mo:base/List";
import Stack "mo:base/Stack";
import Iter "mo:base/Iter";
import Blob "mo:base/Blob";
import Nat "mo:base/Nat";
import Debug "mo:base/Debug";

actor {

  type Maze = [[var Nat8]];

  let hall : Nat8 = 0;
  let wall : Nat8 = 1;

  func visit(n : Nat8) : Nat8 {
    n | 2
  };

  func visited(n : Nat8) : Bool {
    n & 2 == 2
  };

  func bit(b : Bool) : Nat {
    if (b) 1 else 0;
  };

  func chooseMax(f: Random.Finite, max : Nat) : ? Nat {
    assert max > 0;
    do ? {
      if (max == 1) return ? 0;
      var k = bit(f.coin()!);
      var n = max / 2;
      while (n > 1) {
        k := k * 2 + bit(f.coin()!);
        n := n / 2;
      };
      if (k < max)
      	return ? k
      else chooseMax(f, max) !; // retry
    };
  };

  func unvisited(i : Nat, j : Nat, m : Maze) : List.List<(Nat,Nat)> {
    let max = m.size() - 1;
    var cs = List.nil<(Nat,Nat)>();
    if (i > 1 and not visited(m[i - 2][j]))
      cs := List.push((i - 2, j), cs);
    if (i + 1 < max and not visited(m[i + 2][j]))
      cs := List.push((i + 2, j), cs);
    if (j > 1 and not visited(m[i][j - 2]))
      cs := List.push((i, j - 2), cs);
    if (j + 1 < max and not visited(m[i][j + 2]))
      cs := List.push((i, j + 2), cs);
    cs;
  };

  private func dfs(i : Nat, j: Nat, m : Maze) : async  () {
    let s = Stack.Stack<(Nat,Nat)>();
    var f = Random.Finite(await Random.blob());
    m[i][j] := visit(hall);
    s.push((i, j));
    loop {
      switch (s.pop()) {
        case null return;
        case (?(i, j)) {
          let us = unvisited(i, j, m);
          if (not List.isNil(us)) {
            switch (chooseMax(f, List.size(us))) {
              case (? k) {
                s.push((i, j));
                let ? u = List.get(us, k);
                m[if (i == u.0) i else (Nat.min(i, u.0) + 1)]
                  [if (j == u.1) j else (Nat.min(j, u.1) + 1)] := hall;
                m[u.0][u.1] := visit(hall);
                s.push(u);
              };
              case null {
                Debug.print("need more entropy...");
                s.push((i,j));
                f := Random.Finite(await Random.blob());
              }
            }
          }
        }
      }
    }
  };

  func toText(maze : Maze) : Text {
    var t = "";
    for (row in maze.vals()) {
      for (col in row.vals()) {
        t #= if (col == wall) "🟥" else "⬜";
      };
    t #= "\n";
    };
    t
  };

  public func get(n : Nat) : async Text {
    let m = Array.tabulate<[var Nat8]>(2 * n + 1,
      func i { Array.init(2 * n + 1, wall) });
    await dfs(1, 1, m);
    toText(m);
  };

};
