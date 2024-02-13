package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.request.RequestShipment;
import com.proyect.masterdata.dto.request.RequestShipmentItem;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.*;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
@Log4j2
public class ShipmentImpl implements IShipment {

    private final UserRepository userRepository;
    private final ShipmentRepository shipmentRepository;
    private final WarehouseRepository warehouseRepository;
    private final PurchaseRepository purchaseRepository;
    private final IStockTransaction iStockTransaction;
    private final IShipmentItem iShipmentItem;
    private final IWarehouseStock iWarehouseStock;
    private final IGeneralStock iGeneralStock;
    private final ShipmentTypeRepository shipmentTypeRepository;

    @Override
    public ResponseSuccess save(RequestShipment requestShipment, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {

        User user;
        Warehouse warehouse;
        Shipment shipment;
        Purchase purchase;
        ShipmentType shipmentType;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            warehouse = warehouseRepository.findByNameAndStatusTrue(requestShipment.getWarehouse().toUpperCase());
            shipment = shipmentRepository.findBySerial(requestShipment.getPurchaseSerial().toUpperCase());
            purchase = purchaseRepository.findBySerialAndStatusTrue(requestShipment.getPurchaseSerial().toUpperCase());
            shipmentType = shipmentTypeRepository.findByNameAndStatusTrue(requestShipment.getShipmentType().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (warehouse == null) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if (!Objects.equals(warehouse.getClientId(), user.getClientId())) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if (shipment != null) {
            throw new BadRequestExceptions(Constants.ErrorShipmentExists);
        }

        if(purchase == null){
            throw new BadRequestExceptions(Constants.ErrorPurchase);
        }

        if(shipmentType == null){
            throw new BadRequestExceptions(Constants.ErrorShipmentType);
        }

        try{
            List<RequestStockTransactionItem> requestStockTransactionItemList = requestShipment.getRequestShipmentItemList().stream().map(shipmentItem -> RequestStockTransactionItem.builder()
                    .quantity(shipmentItem.getQuantity())
                    .supplierProductSerial(shipmentItem.getSupplierProductSerial().toUpperCase())
                    .build()).toList();
            StockTransaction newStockTransaction = iStockTransaction.save("S"+requestShipment.getPurchaseSerial().toUpperCase(), warehouse.getName(),requestStockTransactionItemList,"ENTRADA",user.getUsername());
            Shipment newShipment = shipmentRepository.save(Shipment.builder()
                            .serial(requestShipment.getPurchaseSerial().toUpperCase())
                            .status(true)
                            .purchase(purchase)
                            .purchaseId(purchase.getId())
                            .stockTransaction(newStockTransaction)
                            .stockTransactionId(newStockTransaction.getId())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .warehouse(warehouse)
                            .warehouseId(warehouse.getId())
                            .shipmentType(shipmentType)
                            .shipmentTypeId(shipmentType.getId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .tokenUser(user.getUsername())
                      .build());
            for(RequestShipmentItem requestShipmentItem : requestShipment.getRequestShipmentItemList()){
                  iShipmentItem.save(newShipment,purchase,warehouse.getName(),requestShipmentItem,user.getUsername());
                  iWarehouseStock.in(warehouse.getName(),requestShipmentItem.getSupplierProductSerial(),requestShipmentItem.getQuantity(),user.getUsername());
                  iGeneralStock.in(requestShipmentItem.getSupplierProductSerial(),requestShipmentItem.getQuantity(),user.getUsername());
            }
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
