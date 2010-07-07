#!/bin/sh

# Swap writable and non-writable config files
sudo mv /etc/vsftpd.conf /etc/vsftpd-tmp.conf
sudo mv /etc/vsftpd-not-active.conf /etc/vsftpd.conf
sudo mv /etc/vsftpd-tmp.conf /etc/vsftpd-not-active.conf
sudo /etc/init.d/vsftpd restart

# Write wether or not the ftp server is writable
if [ -f /etc/vsftpd.conf ] && egrep -iq "^ *write_enable *= *YES" /etc/vsftpd.conf; then
    echo "*** FTP server __writable__ ***"
else
    echo "*** FTP server non writable ***"
fi


