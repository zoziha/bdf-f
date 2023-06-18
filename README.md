<div align='center'>

# MSC.PATRAN BDF 文件解析

![bdf-f](https://img.shields.io/badge/bdf--f-v0.2.20230618-blueviolet)
![Language](https://img.shields.io/badge/-Fortran-734f96?logo=fortran&logoColor=white)
[![license](https://img.shields.io/badge/License-MIT-brightgreen)](LICENSE)

MSC.Patran/Gmsh BDF 网格文件解析，采用面向对象风格。

**欢迎建议与代码贡献！**

</div>

## 使用 [Fortran-lang/fpm](https://github.com/fortran-lang/fpm) 构建

```sh
> fpm run --example --all
read time /sec:  20.33
# BDF file info #
ngrid          : 1016725
ncard3         : 10067
ncard4         : 618010
npload2        : 0
```

```toml
[dependencies]
bdf-f = { git = "https://gitee.com/ship-motions/bdf-f" }
```

## API 文档

```sh
> ford FORD.md
```
