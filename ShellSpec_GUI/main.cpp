#include "shellspec.h"
#include <QApplication>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    shellspec w;
    w.show();

    return a.exec();
}
