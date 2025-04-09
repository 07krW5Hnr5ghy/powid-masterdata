package com.proyect.masterdata.utils;

import com.proyect.masterdata.domain.OrderItem;
import com.proyect.masterdata.domain.ProductPrice;

public class PricingUtil {
    public static Double calculateTotalPrice(OrderItem orderItem, ProductPrice productPrice) {
        return calculateTotalPrice(orderItem.getQuantity(), productPrice, orderItem);
    }

    public static Double calculateTotalPriceUsingPreparedProducts(OrderItem orderItem, ProductPrice productPrice) {
        return calculateTotalPrice(orderItem.getPreparedProducts(), productPrice, orderItem);
    }

    private static Double calculateTotalPrice(double quantity, ProductPrice productPrice, OrderItem orderItem) {
        double unitPrice = productPrice.getUnitSalePrice();
        double discountAmount = orderItem.getDiscountAmount();
        String discountType = orderItem.getDiscount().getName();

        return switch (discountType) {
            case "PORCENTAJE" -> (unitPrice * quantity) - ((unitPrice * quantity) * (discountAmount / 100));
            case "MONTO" -> (unitPrice * quantity) - discountAmount;
            case "NO APLICA" -> unitPrice * quantity;
            default -> null;
        };
    }

}
