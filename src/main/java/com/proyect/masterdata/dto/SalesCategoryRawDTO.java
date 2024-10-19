package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.Date;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class SalesCategoryRawDTO {
    private Long orderId;
    private Date registrationDate;
    private Double orderDiscountAmount;
    private String orderDiscountName;
    private String saleChannelName;
    private Long orderItemId;
    private Integer quantity;
    private Double orderItemDiscountAmount;
    private String orderItemDiscountName;
    private String categoryName;
    private Double unitSalePrice;
    private Double orderItemPrice;
}
