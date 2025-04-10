package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.Model;
import com.proyect.masterdata.domain.OrderItem;
import com.proyect.masterdata.domain.Product;

import java.time.OffsetDateTime;
import java.util.Date;

public interface IUtil {
    OffsetDateTime parseToOffsetDateTime(String input,boolean isStart);
    double calculateTotalPrice(OrderItem orderItem);
    String buildProductSku(Product product);
}
