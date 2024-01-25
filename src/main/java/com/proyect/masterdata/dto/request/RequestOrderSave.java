package com.proyect.masterdata.dto.request;

import com.proyect.masterdata.domain.Item;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestOrderSave {

    private String deliveryMan;

    private String deliveryManPhone;

    private String seller;

    private String observations;

    private String paymentReceipt;

    private String deliveryAddress;

    private Double saleAmount;

    private Double deliveryAmount;

    private Double advancedPayment;

    private String paymentState;

    private String saleChannel;

    private String paymentMethod;

    private String managementType;

    private RequestItem[] requestItems;

    private String customerName;

    private String customerType;

    private String instagram;

    private String customerPhone;

    private String customerAddress;

    private String customerDistrict;

    private String customerProvince;

    private String customerDepartment;

    private String customerReference;

}
