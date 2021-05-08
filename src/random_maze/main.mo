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

    let rand = Blob.fromArray([183,120,32,51,76,77,213,50,167,94,95,38,17,141,42,86,
164,179,149,169,94,248,116,115,132,128,248,75,223,59,117,192]);

    let hall : Nat8 = 0;
    let wall : Nat8 = 1;
    func visit(n : Nat8) : Nat8 { n | 2 };
    func visited(n : Nat8) : Bool { n & 2 == 2 };

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

    private func dfs(i : Nat, j: Nat, m : Maze, f : Random.Finite) : /* async */ () {
        m[i][j] := visit(hall);
        Debug.print(toText(m));
        loop {
          let us = unvisited(i, j, m);
          if (List.isNil(us)) return;
          switch (f.range(2)) {
            case (? k) {
                let ? u = List.get(us, k % List.size(us));
                m[if (i == u.0) i else (Nat.min(i, u.0) + 1)]
                    [if (j == u.1) j else (Nat.min(j, u.1) + 1)] := hall;
              /* await */ dfs(u.0, u.1, m, f);
            };
            case null {
              return /* await */ dfs(i, j, m, Random.Finite(rand /* await Random.blob() */))
            };
          };
        };
    };

    private func iterative(i : Nat, j: Nat, m : Maze) : /* async  */() {
        let s = Stack.Stack<(Nat,Nat)>();
        var f = Random.Finite(rand /* await Random.blob() */);
        m[i][j] := visit(hall); 
        s.push((i, j));
        loop {
            switch (s.pop()) {
                case null return;
                case (?(i, j)) {      
                    let us = unvisited(i, j, m);
                    if (not List.isNil(us)) { 
                        switch (f.range(2)) {
                            case (? k) {
                                s.push((i, j));
                                let ? u = List.get(us, k % List.size(us));
                                m[if (i == u.0) i else (Nat.min(i, u.0) + 1)]
                                    [if (j == u.1) j else (Nat.min(j, u.1) + 1)] := hall;
                                m[u.0][u.1] := visit(hall);
                                s.push(u); 
                            };
                            case null {
                                s.push((i,j));
                                f := Random.Finite(rand /*await Random.blob()*/);
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

    public query func get() : async Text {
        let n = 16;
        let m = Array.tabulate<[var Nat8]>(2 * n + 1,
            func i { Array.init(2 * n + 1, wall) });
        dfs(1, 1, m, Random.Finite(rand/* await Random.blob()*/));
        toText(m);
    };

    public query func gen() : async Text {
        let n = 16;
        let m = Array.tabulate<[var Nat8]>(2 * n + 1,
            func i { Array.init(2 * n + 1, wall) });
        /* await */ iterative(1, 1, m);
        toText(m);
    };
};
