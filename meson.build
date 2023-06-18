project(
    'bdf-f',
    'fortran',
    version: '0.2.20230618',
    license: 'MIT',
    meson_version: '>=0.60.0',
    default_options : [
        'buildtype=debugoptimized',
        'fortran_std=f2008',
        'default_library=both',
    ],
)

subdir('src')
subdir('test')
subdir('example')

bdf_f_lic = files('LICENSE')
install_data(
    bdf_f_lic,
    install_dir: join_paths(get_option('prefix'), 'share', 'licenses', meson.project_name()),
)
