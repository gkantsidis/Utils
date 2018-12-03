namespace CGFSHelper.Units

module Information =
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

    [<Measure>] type symbol
    [<Measure>] type baud = symbol / second

    [<Measure>] type sample
