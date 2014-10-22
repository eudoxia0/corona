function highlightMachineDefinition() {
    const code = document.getElementById('definition-code');
    HighlightLisp.highlight_element(code);
}

function renderCodeExample(sys_name, sys_version, sys_arch) {
    var sys_str = '(:' + sys_name + ' :' + sys_version + ' :' + sys_arch
        + ')';
    sys_str = '(defmachine my-machine\n  :system ' + sys_str + '\n  :memory 1024)\n';
    sys_str += '\n(start my-machine) ;; Bring it up\n';
    sys_str += '\n(stop my-machine) ;; Stop it';
    return sys_str;
}

$('#system-list li').click(function (e) {
    $('#system-list li.active').removeClass('active');
    $(this).addClass('active');
    const name = $(this).attr('data-sys-name');
    const ver = $(this).attr('data-sys-version')
    const arch = $(this).attr('data-sys-arch')
    $('#definition-code').html(renderCodeExample(name, ver, arch));
    highlightMachineDefinition();
})

$(document).ready(function() {
    // Highlight usage code
    $('#usage pre code').each(function(idx, elem) {
        $(elem).addClass('lisp');
    });
    HighlightLisp.highlight_auto();
    // Highlight the machine definition's code
    highlightMachineDefinition();
    // Click on the first item in the list of available systems
    $('#system-list li').first().click();
});
