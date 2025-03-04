package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.List;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestSupplyOrder {
    private String ref;
    private String warehouse;
<<<<<<< HEAD:src/main/java/com/proyect/masterdata/dto/request/RequestSupplyOrder.java
    private OffsetDateTime deliveryDate;
    private List<RequestSupplyOrderItem> requestSupplyOrderItemList;
=======
    private String purchaseType;
    private String purchaseDocument;
    private String supplier;
    private String purchasePaymentType;
    private OffsetDateTime deliveryDate;
    private List<RequestPurchaseItem> requestPurchaseItemList;
>>>>>>> 0ceaf282c4cc63fc1280064498b8b7e9b3e0ca9a:src/main/java/com/proyect/masterdata/dto/request/RequestPurchase.java
}
