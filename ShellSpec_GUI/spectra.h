#ifndef SPECTRA_H
#define SPECTRA_H

#include <QDialog>
#include "shellspec.h"

namespace Ui {
class spectra;
}

class spectra : public QDialog
{
    Q_OBJECT

public:
    shellspec conn;
    void set_default_user_inputs(QString field);
    explicit spectra(QWidget *parent = nullptr);
    ~spectra();

private slots:
    void on_buttonSpectrum1_clicked();
    void on_buttonSpectrum2_clicked();
    void on_buttonSpectrum3_clicked();
    void on_buttonBox_accepted();

private:
    Ui::spectra *ui;
};

#endif // SPECTRA_H
