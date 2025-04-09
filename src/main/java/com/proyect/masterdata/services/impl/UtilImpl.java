package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.OrderItem;
import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.domain.ProductPrice;
import com.proyect.masterdata.repository.ProductPriceRepository;
import com.proyect.masterdata.services.IUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.time.*;
import java.time.format.DateTimeParseException;
import java.util.Objects;

@Service
@RequiredArgsConstructor
@Log4j2
public class UtilImpl implements IUtil {
    private final ProductPriceRepository productPriceRepository;
    @Override
    public OffsetDateTime parseToOffsetDateTime(String input, boolean isStart) {
        ZoneOffset defaultOffset = ZoneId.systemDefault().getRules().getOffset(Instant.now());
        if (input == null) return null;
        try {
            return OffsetDateTime.parse(input);
        } catch (DateTimeParseException e) {
            try {
                LocalDate date = LocalDate.parse(input);
                LocalTime time = isStart ? LocalTime.MIN : LocalTime.MAX;
                return OffsetDateTime.of(date, time, defaultOffset);
            } catch (DateTimeParseException ex) {
                throw new IllegalArgumentException("Invalid date format: " + input);
            }
        }
    }
    @Override
    public double calculateTotalPrice(OrderItem orderItem) {
        ProductPrice productPrice = productPriceRepository.findByProductId(orderItem.getProductId());
        double totalPrice = productPrice.getUnitSalePrice() * orderItem.getQuantity();

        if (Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")) {
            totalPrice -= totalPrice * (orderItem.getDiscountAmount() / 100);
        } else if (Objects.equals(orderItem.getDiscount().getName(), "MONTO")) {
            totalPrice -= orderItem.getDiscountAmount();
        }

        return totalPrice;
    }

    @Override
    public String buildProductSku(Product product) {
        return product.getModel().getBrand().getSku()
                + product.getSubCategoryProduct().getCategoryProduct().getSku()
                + product.getSubCategoryProduct().getSku()
                + product.getModel().getSku()
                + product.getColor().getSku()
                + product.getSize().getName();
    }

}
