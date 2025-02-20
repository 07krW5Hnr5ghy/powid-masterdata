package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.OrderItem;
import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.domain.ProductPrice;
import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.repository.ProductPriceRepository;
import com.proyect.masterdata.services.IUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

@Service
@RequiredArgsConstructor
@Log4j2
public class UtilImpl implements IUtil {

    private final ProductPriceRepository productPriceRepository;

    @Override
    public Date setToUTCStartOfDay(Date date) {
        Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone("GMT-5"));
        calendar.setTime(date);
        calendar.set(Calendar.HOUR_OF_DAY, 0);
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        calendar.add(Calendar.DAY_OF_MONTH, 1);
        return calendar.getTime();
    }

    @Override
    public Date setToUTCEndOfDay(Date date) {
        Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone("GMT-5"));
        calendar.setTime(date);
        calendar.set(Calendar.HOUR_OF_DAY, 23);
        calendar.set(Calendar.MINUTE, 59);
        calendar.set(Calendar.SECOND, 59);
        calendar.set(Calendar.MILLISECOND, 999);
        calendar.add(Calendar.DAY_OF_MONTH, 1);
        return calendar.getTime();
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

    @Override
    public String buildInventorySku(SupplierProduct supplierProduct) {
        return supplierProduct.getProduct().getModel().getBrand().getSku()
                + supplierProduct.getProduct().getSubCategoryProduct().getCategoryProduct().getSku()
                + supplierProduct.getProduct().getSubCategoryProduct().getSku()
                + supplierProduct.getProduct().getModel().getSku()
                + supplierProduct.getProduct().getColor().getSku()
                + supplierProduct.getProduct().getSize().getName()
                + supplierProduct.getSupplier().getSku();
    }
}
