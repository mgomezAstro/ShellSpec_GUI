#ifndef ABOUT_VERSION_H
#define ABOUT_VERSION_H

#include <QDialog>

namespace Ui {
class About_version;
}

class About_version : public QDialog
{
    Q_OBJECT

public:
    explicit About_version(QWidget *parent = nullptr);
    ~About_version();

private:
    Ui::About_version *ui;
};

#endif // ABOUT_VERSION_H
