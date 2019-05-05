#include <ecl/ecl.h>

int main(int argc, char **argv)
{
    cl_boot(argc, argv);
    cl_eval(c_string_to_object("(defun hw ()"
                               "  (write-line \"Hello World!\")"
                               "  (values))"));
    cl_eval(c_string_to_object("(hw)"));
    cl_shutdown();
    return 0;
}
