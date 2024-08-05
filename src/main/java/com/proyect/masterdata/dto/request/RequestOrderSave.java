package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestOrderSave {
    private String observations;
    private String deliveryAddress;
    private Double deliveryAmount;
    private Double advancedPayment;
    private String saleChannel;
    private String paymentMethod;
    private String managementType;
    private List<RequestOrderItem> requestOrderItems;
    private String storeName;
    private String closingChannel;
    private String deliveryPoint;
    private String discount;
    private Double discountAmount;
    private String phone;
    private String dni;
}
