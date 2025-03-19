package com.proyect.masterdata.dto.projections;

import com.proyect.masterdata.domain.DeliveryManifest;
import com.proyect.masterdata.domain.OrderItem;
import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.domain.User;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

public interface DeliveryManifestItemDTOP {
    UUID getId();
    Integer getQuantity();
    OrderItem getOrderItem();
    DeliveryManifest getDeliveryManifest();
    Product getProduct();
    User getUser();
    boolean isDelivered();
}
