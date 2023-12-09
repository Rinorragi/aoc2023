# Advent of Code 2023

Repository to my puzzle solutions for [https://adventofcode.com/2023](https://adventofcode.com/2023).

## Setup

What was required to put the repository up. [Download .NET 8 SDK](https://dotnet.microsoft.com/en-us/download/dotnet/8.0).

```powershell
dotnet new gitignore
```

## How to run

You need to add inputs to input folder with dayXX.txt or dayXX_example.txt (also sometimes dayXX_exampleN.txt).

```powershell
dotnet fsi dayX.fsx
```

or to run all simply 
```powershell
ls -filter *.fsx | % { dotnet fsi $_ }
```

## Speed

Just for fun some kind of table of times on my rig. 

| Day | Time taken   |
| --- | ------------ |
|   1 | 00:00:00.062 |
|   2 | 00:00:00.005 |
|   3 | 00:00:00.011 |
|   4 | 00:00:06.080 |
|   5 | 00:00:02.998 |
|   6 | 00:00:01.886 |
|   7 | 00:00:00.019 |
|   8 | 00:00:00.017 |
|   9 | 00:00:00.007 |