(function () {

  // =========================================================================
  // Filepicker.io and Ace editor

  var UPLOAD_SELECTOR = '#source .upload'
    , FP_SERVICES     = ['COMPUTER', 'DROPBOX', 'GITHUB', 'FTP'];

  // Trigger a Filepicker.io 'pick file' modal
  // @param   click event that triggered this
  var pickFile = function (event) {
    filepicker.pick({
      extension: '.asm'
    , container: 'modal'
    , services: FP_SERVICES
    }, updateEditor
    , onFPError
    );
  };

  // Initialize the editor, grab the file contents, and update the editor
  // @param   FilePicker.io file object
  var updateEditor = function (FPFile) {
    var editor  = ace.edit('editor')
      , $editor = $('#editor');

    editor.setTheme('ace/theme/tomorrow');
    $editor.removeclass('placeholder');

    filepicker.read(FPFile, function (data) {
      console.log("Got file..." + data);
      editor.setValue(data);
    });
  };

  // Handle an error from the Filepicker.io iAPI
  // @param   Filepicker.io error object
  var onFPError = function (FPError) {
    console.log(FPError.toString());
  };

  // =========================================================================
  // DOM ready
  $(function () {

    $(UPLOAD_SELECTOR).on('click', pickFile);

  });

})();
