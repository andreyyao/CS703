from pathlib import Path
import shutil
import numpy as np
OUT_DIR = Path("challenge_examples")


def make_single_lambda_examples(max_depth, side="left"):
    template = "lambda a:Int.{|%%%HOLE%%%|}"

    # Int
    # (Int*Int)
    # (Int*(Int*Int))
    # (Int*(Int*(Int*Int)))
    prev = "Int"
    out = []
    actual_side = side
    rng = np.random.RandomState(0)
    for i in range(0, max_depth):
        if side == "random":
            actual_side = rng.choice(["right", "left"])
        
        if actual_side == "left":
            new_hole = f"(Int * {prev})"
        else:
            new_hole = f"({prev} * Int)"
            
        out.append(new_hole)
        prev = new_hole
    return [template.replace("%%%HOLE%%%", a) for a in out]


def make_multi_lambda(max_depth):
    out = [
        "lambda f0:Int->Bool.lambda x:Int.{|Bool|}",
        "lambda f0:Int->Bool.lambda g0:Bool->(Bool*Int).lambda x:Int.{|(Bool*Int)|}",
    ]
    

    prev_type = "(Bool*Int)"
    prev_funcs = "lambda f0:Int->Bool.lambda g0:Bool->(Bool*Int)"
    g_count = 1
    f_count = 1
    for i in range(len(out), max_depth):
        if i % 2 == 0:
            
            if g_count % 2 == 0:
                new_type = f"(Int*{prev_type})"
            else:
                new_type = f"({prev_type}*Int)"
        
            new_func = f"g{g_count}:{prev_type}->{new_type}"
            g_count += 1
        else:
            if f_count % 2 == 0:
                new_type = f"(Bool*{prev_type})"
            else:
                new_type = f"({prev_type}*Bool)"
                
            new_func = f"f{f_count}:{prev_type}->{new_type}"
            f_count += 1
        hole = "{|"+new_type+"|}"
        prev_funcs = f"{prev_funcs}.lambda {new_func}"
        out.append(f"{prev_funcs}.lambda x:Int.{hole}")
        prev_type = new_type
    return out


def make_pair_manip_example(max_depth,side="left"):
    if side in ["right", "evens"]:
        out = [
            "lambda x:Bool.{|Bool|}",
            "lambda x:(Bool*Int).{|Bool|}",
        ]
        prev = "(Bool*Int)"
    else:
        out = [
            "lambda x:Bool.{|Bool|}",
            "lambda x:(Int*Bool).{|Bool|}",
        ]
        prev="(Int*Bool)"
    
    rng = np.random.RandomState(0)
    template = "lambda x:%%%TYPE%%%.{|Bool|}"
    
    actual_side = side 
    for _ in range(len(out), max_depth):
        if side == "random":
            actual_side = rng.choice(["right", "left"])
        if actual_side == "left":
            new_type = f"(Int*{prev})"
        else:
            new_type = f"({prev}*Int)"
        
        out.append(template.replace("%%%TYPE%%%", new_type))
        prev = new_type
    return out


def main():
    depth = 5
    if OUT_DIR.exists():
        shutil.rmtree(OUT_DIR)
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    for i, e in enumerate(make_single_lambda_examples(depth,side="left")):
        with open(OUT_DIR / f"left_single-{i+1}.txt", "w") as f:
            f.write(e)
    # for i, e in enumerate(make_single_lambda_examples(depth,side="right")):
    #     with open(OUT_DIR / f"right_single-{i+1}.txt", "w") as f:
    #         f.write(e)
    # for i, e in enumerate(make_single_lambda_examples(depth,"random")):
    #     with open(OUT_DIR / f"rng_single-{i+1}.txt", "w") as f:
    #         f.write(e)

    for i, e in enumerate(make_multi_lambda(depth)):
        with open(OUT_DIR / f"multi-{i+1}.txt", "w") as f:
            f.write(e)
    
    for i, e in enumerate(make_pair_manip_example(depth, "left")):
        with open(OUT_DIR / f"left_pair-{i+1}.txt", "w") as f:
            f.write(e)
        
    # for i, e in enumerate(make_pair_manip_example(depth, "right")):
    #     with open(OUT_DIR / f"right_pair-{i+1}.txt", "w") as f:
    #         f.write(e)
    
    # for i, e in enumerate(make_pair_manip_example(depth, "random")):
    #     with open(OUT_DIR / f"rng_pair{i+1}.txt", "w") as f:
    #         f.write(e)

if __name__ == "__main__":
    main()
