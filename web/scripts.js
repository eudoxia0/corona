function renderCodeExample(sys_name, sys_version, sys_arch) {
    const sys_str = '(:' + sys_name + ' :' + sys_version + ' :' + sys_arch
        + ')';
    return '(defmachine my-machine\n  :system ' + sys_str + '\n  :memory 1024)';
}

$('#system-list a').click(function (e) {
    $('#system-list a.active').removeClass('active');
    $(this).addClass('active');
    const name = $(this).attr('data-sys-name');
    const ver = $(this).attr('data-sys-version')
    const arch = $(this).attr('data-sys-arch')
    $('#machine-definition').html(renderCodeExample(name, ver, arch));
})
