package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.request.RequestShipmentItem;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IGeneralStock;
import com.proyect.masterdata.services.IShipment;
import com.proyect.masterdata.services.IStockTransaction;
import com.proyect.masterdata.services.IWarehouseStock;
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

    @Override
    public ResponseSuccess save(String serial, String warehouse, List<RequestShipmentItem> requestShipmentItemList, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {

        User user;
        Warehouse warehouseData;
        Shipment shipment;
        Purchase purchase;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            warehouseData = warehouseRepository.findByNameAndStatusTrue(warehouse.toUpperCase());
            shipment = shipmentRepository.findBySerial(serial.toUpperCase());
            purchase = purchaseRepository.findBySerialAndStatusTrue(serial.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (warehouseData == null) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if (!Objects.equals(warehouseData.getClientId(), user.getClientId())) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if (shipment != null) {
            throw new BadRequestExceptions(Constants.ErrorShipmentExists);
        }

        try{
              Shipment newShipment = shipmentRepository.save(Shipment.builder()
                              .serial(serial.toUpperCase())
                              .status(true)
                              .registrationDate(new Date(System.currentTimeMillis()))
                              .updateDate(new Date(System.currentTimeMillis()))
                              .client(user.getClient())
                              .clientId(user.getClientId())
                              .tokenUser(user.getUsername())
                      .build());
              List<RequestStockTransactionItem> requestStockTransactionItemList = requestShipmentItemList.stream().map(shipmentItem -> RequestStockTransactionItem.builder()
                      .quantity(shipmentItem.getQuantity())
                      .supplierProductSerial(shipmentItem.getSupplierProductSerial().toUpperCase())
                      .build()).toList();
              StockTransaction newStockTransaction = iStockTransaction.save(serial.toUpperCase(),warehouseData.getName(),requestStockTransactionItemList,"ENTRADA",user.getUsername());
              for(RequestShipmentItem requestShipmentItem : requestShipmentItemList){


              }
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        return null;
    }
}
