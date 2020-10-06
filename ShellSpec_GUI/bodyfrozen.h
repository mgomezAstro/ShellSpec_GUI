#ifndef BODYFROZEN_H
#define BODYFROZEN_H

#include <QDialog>
#include "shellspec.h"

namespace Ui {
class bodyfrozen;
}

class bodyfrozen : public QDialog
{
    Q_OBJECT

public:
    shellspec conn;
    void set_default_user_inputs(QString field);
    explicit bodyfrozen(QWidget *parent = nullptr);
    ~bodyfrozen();

private slots:
    void on_buttonBox_accepted();

private:
    Ui::bodyfrozen *ui;
};

#endif // BODYFROZEN_H
