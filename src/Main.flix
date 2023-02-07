enum Piece with Eq, Order, ToString {
    case King, Queen, Rook, Bishop, Knight
}

type alias Position = (Int32, Int32)
type alias Solution = Set[(Piece, Position)]

def takes(p: Piece, delta: Position): Bool =
    let (dx, dy) = delta;
    match p {
        case King => dx <= 1 and dy <= 1,
        case Queen => dx == 0 or dy == 0 or dx == dy,
        case Rook => dx == 0 or dy == 0,
        case Bishop => dx == dy,
        case Knight => (dx == 1 and dy == 2) or (dx == 2 and dy == 1)
    }

def allows(p: Piece, pos1: Position, pos2: Position): Bool =
    let (x1, y1) = pos1;
    let (x2, y2) = pos2;
    let delta = (Int32.abs(x1 - x2), Int32.abs(y1 - y2));
    pos1 != pos2 and not takes(p, delta)

def allowed(candidate: Piece, pos1: Position, solution: Solution): Bool =
    solution |> Set.forAll(
        match (piece, pos2) -> allows(candidate, pos1, pos2) and allows(piece, pos2, pos1))

def solve(rows: Int32, cols: Int32, pieces: List[Piece]): Set[Solution] =
    match pieces {
        case Nil => Set#{Set#{}}
        case piece :: rest =>
            foreach (
                solution <- solve(rows, cols, rest);
                x <- Set.range(0, rows);
                y <- Set.range(0, cols);
                pos <- Some((x, y));
                if allowed(piece, pos, solution)
            ) yield Set.insert((piece, pos), solution)
    }

def main(): Unit & Impure =
    // let solutions = solve(3, 3, List#{King, King, Rook});
    // let solutions = solve(4, 4, List#{Rook, Rook, Knight, Knight, Knight, Knight});
    let solutions = solve(6, 9, List#{King, King, Knight, Rook, Bishop, Queen});
    println(Set.size(solutions))
