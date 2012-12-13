(function () {

  UPLOAD_SELECTOR = '#upload';
  FP_SERVICES = ['COMPUTER', 'DROPBOX', 'GITHUB', 'FTP'];

  var updateEditor = function (FPFile) {
    var editor = ace.edit('editor');
    editor.setTheme('ace/theme/tomorrow');

    filepicker.read(FPFile, function (data) {
      editor.setValue(data);
    });
  };

  var onFPError = function (FPError) {
    console.log(FPError.toString());
  };

  // DOM ready
  $(function () {

    $(UPLOAD_SELECTOR).on('click', function () {
      filepicker.pick({
        extension: '.asm'
      , container: 'modal'
      , services: FP_SERVICES
      }, updateEditor
      , onFPError
      );
    });
  });

})();
