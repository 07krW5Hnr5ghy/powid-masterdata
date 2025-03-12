package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.OrderItem;
import com.proyect.masterdata.domain.Product;

import java.util.Date;

public interface IUtil {
    Date setToUTCStartOfDay(Date date);
    Date setToUTCEndOfDay(Date date);
    double calculateTotalPrice(OrderItem orderItem);
    String buildProductSku(Product product);
    String getUniqueProductKey(Product product);
}
