package com.proyect.masterdata.services.impl;

import java.util.*;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.domain.WarehouseStock;
import com.proyect.masterdata.dto.WarehouseDTO;
import com.proyect.masterdata.dto.WarehouseStockDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IWarehouseStock;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class WarehouseStockImpl implements IWarehouseStock {

    private final WarehouseStockRepository warehouseStockRepository;
    private final UserRepository userRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final WarehouseRepository warehouseRepository;
    private final WarehouseStockRepositoryCustom warehouseStockRepositoryCustom;
    private final SupplierRepository supplierRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess in(Warehouse warehouse, SupplierProduct supplierProduct, Integer quantity, User user)
            throws InternalErrorExceptions, BadRequestExceptions {

        WarehouseStock warehouseStock;

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (supplierProduct == null) {
            throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
        }

        if (warehouse == null) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if (!Objects.equals(warehouse.getClientId(), user.getClientId())) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        try {
            warehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(warehouse.getId(),
                    supplierProduct.getId());

            if (warehouseStock != null) {
                warehouseStock.setQuantity(warehouseStock.getQuantity() + quantity);
                warehouseStock.setUpdateDate(new Date(System.currentTimeMillis()));
                warehouseStock.setTokenUser(user.getUsername());
                warehouseStockRepository.save(warehouseStock);
            } else {
                warehouseStockRepository.save(WarehouseStock.builder()
                        .quantity(quantity)
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .supplierProduct(supplierProduct)
                        .supplierProductId(supplierProduct.getId())
                        .warehouse(warehouse)
                        .warehouseId(warehouse.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .tokenUser(user.getUsername())
                        .build());
            }
            iAudit.save("ADD_WAREHOUSE_STOCK","INGRESAN ("+quantity+") UNIDADES DE STOCK DE PRODUCTO DE INVENTARIO "+supplierProduct.getSerial()+" EN ALMACEN "+warehouse.getName()+".",warehouse.getName(),user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseSuccess out(Warehouse warehouse, SupplierProduct supplierProduct, Integer quantity, User user)
            throws InternalErrorExceptions, BadRequestExceptions {

        WarehouseStock warehouseStock;

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (supplierProduct == null) {
            throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
        }

        if (warehouse == null) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if (!Objects.equals(warehouse.getClientId(), user.getClientId())) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        try {
            warehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(warehouse.getId(),
                    supplierProduct.getId());

            if (warehouseStock == null) {
                throw new BadRequestExceptions(Constants.ErrorWarehouseStock);
            }

            if (quantity > warehouseStock.getQuantity()) {
                throw new BadRequestExceptions(Constants.ErrorWarehouseStockLess);
            }

            warehouseStock.setQuantity(warehouseStock.getQuantity() - quantity);
            warehouseStockRepository.save(warehouseStock);
            iAudit.save("DELETE_WAREHOUSE_STOCK","SALIDA DE ("+quantity+") UNIDADES DE STOCK DE PRODUCTO DE INVENTARIO "+supplierProduct.getSerial()+" PARA ALMACEN "+warehouse.getName()+".",warehouse.getName(),user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<Page<WarehouseStockDTO>> list(
            List<String> warehouses,
            String serial,
            String productSku,
            String model,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<WarehouseStock> warehouseStockPage;
            Long clientId;
            List<Long> warehouseIds;

            if(warehouses!=null && !warehouses.isEmpty()){
                warehouseIds = warehouseRepository
                        .findByNameIn(
                                warehouses.stream().map(String::toUpperCase).toList()
                        ).stream().map(Warehouse::getId).toList();
            }else{
                warehouseIds = new ArrayList<>();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                warehouseStockPage = warehouseStockRepositoryCustom.searchForWarehouseStock(
                        clientId,
                        warehouseIds,
                        serial,
                        productSku,
                        model,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (warehouseStockPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<WarehouseStockDTO> warehouseStockDTOs = warehouseStockPage.getContent().stream()
                    .map(warehouseStock -> WarehouseStockDTO.builder()
                            .quantity(warehouseStock.getQuantity())
                            .supplierProduct(warehouseStock.getSupplierProduct().getSerial())
                            .product(warehouseStock.getSupplierProduct().getProduct().getSku())
                            .supplier(warehouseStock.getSupplierProduct().getSupplier().getBusinessName())
                            .model(warehouseStock.getSupplierProduct().getProduct().getModel().getName())
                            .color(warehouseStock.getSupplierProduct().getProduct().getColor().getName())
                            .size(warehouseStock.getSupplierProduct().getProduct().getSize().getName())
                            .warehouse(warehouseStock.getWarehouse().getName())
                            .registrationDate(warehouseStock.getRegistrationDate())
                            .updateDate(warehouseStock.getUpdateDate())
                            .build())
                    .toList();

            return new PageImpl<>(warehouseStockDTOs, warehouseStockPage.getPageable(),
                    warehouseStockPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<WarehouseStockDTO>> listWarehouse(String user,String warehouse,String supplierProduct) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<WarehouseStock> warehouseStocks;
            Long clientId;
            Long warehouseId;
            Long supplierProductId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                if(warehouse != null){
                    warehouseId = warehouseRepository.findByNameAndStatusTrue(warehouse.toUpperCase()).getId();
                }else{
                    warehouseId = null;
                }
                if(supplierProduct != null){
                    supplierProductId = supplierProductRepository.findBySerialAndStatusTrue(supplierProduct.toUpperCase()).getId();
                }else{
                    supplierProductId = null;
                }
                if(warehouseId != null && supplierProductId != null){
                    warehouseStocks = warehouseStockRepository.findAllByWarehouseIdAndSupplierProductId(warehouseId,supplierProductId);
                }else if(warehouseId != null){
                    warehouseStocks = warehouseStockRepository.findAllByClientIdAndWarehouseId(clientId, warehouseId);
                }else{
                    warehouseStocks = warehouseStockRepository.findAllByClientId(clientId);
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(warehouseStocks.isEmpty()){
                return Collections.emptyList();
            }

            return warehouseStocks.stream()
                    .map(warehouseStock -> WarehouseStockDTO.builder()
                            .quantity(warehouseStock.getQuantity())
                            .supplierProduct(warehouseStock.getSupplierProduct().getSerial())
                            .product(warehouseStock.getSupplierProduct().getProduct().getSku())
                            .warehouse(warehouseStock.getWarehouse().getName())
                            .supplier(warehouseStock.getSupplierProduct().getSupplier().getBusinessName())
                            .model(warehouseStock.getSupplierProduct().getProduct().getModel().getName())
                            .color(warehouseStock.getSupplierProduct().getProduct().getColor().getName())
                            .size(warehouseStock.getSupplierProduct().getProduct().getSize().getName())
                            .registrationDate(warehouseStock.getRegistrationDate())
                            .updateDate(warehouseStock.getUpdateDate())
                            .build())
                    .toList();
        });
    }

    @Override
    public CompletableFuture<List<WarehouseStockDTO>> listWarehouseAndSupplier(String user, String warehouse, String supplier) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<WarehouseStock> warehouseStocks;
            Long clientId;
            Long warehouseId;
            Long supplierId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                if(warehouse != null){
                    warehouseId = warehouseRepository.findByNameAndStatusTrue(warehouse.toUpperCase()).getId();
                }else{
                    warehouseId = null;
                }
                if(supplier != null){
                    supplierId = supplierRepository.findByClientIdAndRucAndStatusTrue(clientId,supplier).getId();
                }else{
                    supplierId = null;
                }
                if(warehouseId != null && supplierId != null){
                    warehouseStocks = warehouseStockRepository.findByClientIdAndWarehouseIdAndSupplierProduct_Supplier_Id(clientId,warehouseId,supplierId);
                }else if(warehouseId != null){
                    warehouseStocks = warehouseStockRepository.findAllByClientIdAndWarehouseId(clientId, warehouseId);
                }else{
                    warehouseStocks = warehouseStockRepository.findAllByClientId(clientId);
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(warehouseStocks.isEmpty()){
                return Collections.emptyList();
            }

            return warehouseStocks.stream()
                    .filter(data -> data.getQuantity() > 0)
                    .map(warehouseStock -> WarehouseStockDTO.builder()
                            .quantity(warehouseStock.getQuantity())
                            .supplierProduct(warehouseStock.getSupplierProduct().getSerial())
                            .product(warehouseStock.getSupplierProduct().getProduct().getSku())
                            .warehouse(warehouseStock.getWarehouse().getName())
                            .supplier(warehouseStock.getSupplierProduct().getSupplier().getBusinessName())
                            .model(warehouseStock.getSupplierProduct().getProduct().getModel().getName())
                            .color(warehouseStock.getSupplierProduct().getProduct().getColor().getName())
                            .size(warehouseStock.getSupplierProduct().getProduct().getSize().getName())
                            .registrationDate(warehouseStock.getRegistrationDate())
                            .updateDate(warehouseStock.getUpdateDate())
                            .build())
                    .toList();
        });
    }

}
