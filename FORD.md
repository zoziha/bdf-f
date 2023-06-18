---
project: 🌓bdf-f
summary: MSC.Patran/Gmsh BDF File I/O <br>
         MSC.Patran/Gmsh BDF 网格文件解析
src_dir: src
preprocess: false
project_website: https://gitee.com/ship-motions/bdf-f
project_download: https://gitee.com/ship-motions/bdf-f/releases
output_dir: _build/ford
author: 左志华
author_description: 哈尔滨工程大学-船舶与海洋结构物设计制造，在读学生
email: zuo.zhihua@qq.com
website: https://gitee.com/zoziha
display: public
         private
source: true
parallel: 2
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
---

@note 船海结构设计时，有时候需要将自定义模型的压强数值导入到 MSC.Patran 中进行后续分析，
而 MSC.Patran 的网格文件格式为 BDF，`bdf-f` 项目来源于此。

相关项目：[stl-fortran](https://github.com/jacobwilliams/stl-fortran).
