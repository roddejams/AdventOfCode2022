export enum Result { CorrectOrder, IncorrectOrder, Continue }

var result = compare([1,1,3,1,1], [1,1,5,1,1]);


export function compare(left: any, right: any) : Result {
    if (typeof left == "number" && typeof right == "number") {
        return compareTwoInts(left, right);
    }

    if (typeof left == "number" && Array.isArray(right)) {
        return compareTwoArrays([left], right);
    }

    if (typeof right == "number" && Array.isArray(left)) {
        return compareTwoArrays(left, [right]);
    }

    return compareTwoArrays(left, right);
}

function compareTwoInts(left: number, right: number) : Result {
    if (left == right) {
        return Result.Continue;
    }

    if (left < right) {
        return Result.CorrectOrder;
    }

    return Result.IncorrectOrder;
}

function compareTwoArrays(left: Array<any>, right: Array<any>) : Result {
    if (left.length < right.length) {
        return Result.CorrectOrder;
    }

    if (left.length > right.length) {
        return Result.IncorrectOrder;
    }

    for (let i=0; i < left.length; i++) {
        var leftVal : any = left[i];
        var rightVal : any = right[i];

        var comparison = compare(leftVal, rightVal);
        if (comparison != Result.Continue) {
            return comparison;
        }
    }

    return Result.Continue;
}