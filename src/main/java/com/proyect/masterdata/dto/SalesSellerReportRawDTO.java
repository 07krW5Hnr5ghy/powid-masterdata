package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class SalesSellerReportRawDTO {
    private UUID orderId;
    private OffsetDateTime registrationDate;
    private Double orderDiscountAmount;
    private String orderDiscountName;
    private String closingChannelName;
    private Long orderItemId;
    private Integer quantity;
    private Double orderItemDiscountAmount;
    private String orderItemDiscountName;
    private String categoryName;
    private Double unitSalePrice;
    private Double orderItemPrice;
    private String brandName;
    private String seller;
    private String department;
    private String province;
    private String district;
    private String orderState;
    private Boolean orderItemStatus;
}
