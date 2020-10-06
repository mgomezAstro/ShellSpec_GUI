#include "about_version.h"
#include "ui_about_version.h"

About_version::About_version(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::About_version)
{
    ui->setupUi(this);
}

About_version::~About_version()
{
    delete ui;
}
