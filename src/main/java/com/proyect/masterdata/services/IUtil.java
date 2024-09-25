package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.OrderItem;

import java.util.Date;

public interface IUtil {
    Date setToUTCStartOfDay(Date date);
    double calculateTotalPrice(OrderItem orderItem);
}
