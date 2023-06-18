bdf_f_file = [
    'bdf_keyword.f90',
    'bdf_kinds.f90',
    'bdf_module.f90',
    'bdf_pload2_vector.f90',
    'bdf_utils.f90',
]

bdf_f_lib = library(
    meson.project_name(),
    sources: files(bdf_f_file),
    version: meson.project_version(),
    install: true,
)

bdf_f_inc = bdf_f_lib.private_dir_include()
bdf_f_dep = declare_dependency(
    link_with: bdf_f_lib,
    include_directories: bdf_f_inc,
)

if host_machine.system() == 'windows'
    symbols_file = 'lib'+meson.project_name()+'-'+meson.project_version().split('.')[0]+'.dll.symbols'
    obj_suffix = '.obj'
else
    symbols_file = 'lib'+meson.project_name()+'.so.'+meson.project_version()+'.symbols'
    obj_suffix = '.o'
endif

bdf_f_obj = []
foreach source: bdf_f_file
    bdf_f_obj += source+obj_suffix
endforeach

install_subdir(
    bdf_f_lib.full_path()+'.p',
    install_dir: 'include'/meson.project_name(),
    strip_directory: true,
    exclude_files: [
        'depscan.dd',
        meson.project_name()+'-deps.json',
        symbols_file,
        meson.project_name()+'.dat',
        bdf_f_obj,
    ]
)

pkg = import('pkgconfig')
pkg.generate(
    name : meson.project_name(),
    description : 'MSC.Patran BDF File IO',
    version : meson.project_version(),
    libraries : bdf_f_lib,
    subdirs : meson.project_name(),
)
