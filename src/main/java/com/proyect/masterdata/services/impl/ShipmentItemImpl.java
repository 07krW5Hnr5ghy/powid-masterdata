package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;

import com.proyect.masterdata.domain.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.dto.ShipmentItemDTO;
import com.proyect.masterdata.dto.request.RequestShipmentItem;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.PurchaseItemRepository;
import com.proyect.masterdata.repository.ShipmentItemRepository;
import com.proyect.masterdata.repository.ShipmentItemRepositoryCustom;
import com.proyect.masterdata.repository.SupplierProductRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.WarehouseRepository;
import com.proyect.masterdata.services.IGeneralStock;
import com.proyect.masterdata.services.IShipmentItem;
import com.proyect.masterdata.services.IWarehouseStock;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class ShipmentItemImpl implements IShipmentItem {

    private final UserRepository userRepository;
    private final ShipmentItemRepository shipmentItemRepository;
    private final WarehouseRepository warehouseRepository;
    private final PurchaseItemRepository purchaseItemRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final ShipmentItemRepositoryCustom shipmentItemRepositoryCustom;
    private final IWarehouseStock iWarehouseStock;
    private final IGeneralStock iGeneralStock;

    @Override
    public ShipmentItem save(Shipment shipment,Purchase purchase, String warehouse, RequestShipmentItem requestShipmentItem,
            String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        SupplierProduct supplierProduct;
        ShipmentItem shipmentItem;
        PurchaseItem purchaseItem;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestShipmentItem.getSupplierProductSerial().toUpperCase());
            shipmentItem = shipmentItemRepository.findByShipmentIdAndSupplierProductId(shipment.getId(),supplierProduct.getId());
            purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            e.printStackTrace();
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(supplierProduct == null){
            throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
        }

        if (shipmentItem != null) {
            throw new BadRequestExceptions(Constants.ErrorShipmentExists);
        }

        if(purchaseItem == null){
            throw new BadRequestExceptions(Constants.ErrorPurchaseItem);
        }

        try {

            ShipmentItem newShipmentItem = shipmentItemRepository.save(ShipmentItem.builder()
                            .purchaseItem(purchaseItem)
                            .purchaseItemId(purchaseItem.getId())
                            .shipment(shipment)
                            .shipmentId(shipment.getId())
                            .supplierProduct(supplierProduct)
                            .supplierProductId(supplierProduct.getId())
                            .status(true)
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .observations(requestShipmentItem.getObservations().toUpperCase())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .quantity(requestShipmentItem.getQuantity())
                    .build());

            iWarehouseStock.in(shipment.getWarehouse().getName(),supplierProduct.getSerial(), requestShipmentItem.getQuantity(), user.getUsername());
            iGeneralStock.in(supplierProduct.getSerial(), requestShipmentItem.getQuantity(), user.getUsername());

            return newShipmentItem;
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            e.printStackTrace();
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<ShipmentItemDTO> list(String serial, String user, String warehouse, String sort, String sortColumn,
                                      Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        Page<ShipmentItem> pageShipment;
        Long clientId;
        Long warehouseId;

        if (warehouse != null) {
            warehouseId = warehouseRepository.findByNameAndStatusTrue(warehouse.toUpperCase()).getId();
        } else {
            warehouseId = null;
        }

        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            pageShipment = shipmentItemRepositoryCustom.searchForShipment(clientId, serial, warehouseId, sort, sortColumn,
                    pageNumber, pageSize);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.ResultsFound);
        }

        if (pageShipment.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        List<ShipmentItemDTO> shipmentItemDTOS = pageShipment.getContent().stream().map(shipmentItem -> ShipmentItemDTO.builder()
                .purchaseSerial(shipmentItem.getPurchaseItem().getPurchase().getSerial())
                .quantity(shipmentItem.getQuantity())
                .serial(shipmentItem.getShipment().getSerial())
                .supplierProductSerial(shipmentItem.getSupplierProduct().getSerial())
                .warehouse(shipmentItem.getShipment().getWarehouse().getName())
                .date(shipmentItem.getRegistrationDate())
                .build()).toList();

        return new PageImpl<>(shipmentItemDTOS, pageShipment.getPageable(), pageShipment.getTotalElements());
    }

}
