package com.proyect.masterdata.services.impl;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IUtil;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.domain.WarehouseStock;
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
    private final WarehouseRepository warehouseRepository;
    private final WarehouseStockRepositoryCustom warehouseStockRepositoryCustom;
    private final IAudit iAudit;
    private final IUtil iUtil;
    @Override
    public ResponseSuccess in(Warehouse warehouse, Product product, Integer quantity, User user)
            throws InternalErrorExceptions, BadRequestExceptions {

        WarehouseStock warehouseStock;

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (product == null) {
            throw new BadRequestExceptions(Constants.ErrorProduct);
        }

        if (warehouse == null) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if (!Objects.equals(warehouse.getClientId(), user.getClientId())) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        try {
            warehouseStock = warehouseStockRepository.findByWarehouseIdAndProductId(warehouse.getId(),
                    product.getId());

            if (warehouseStock != null) {
                warehouseStock.setQuantity(warehouseStock.getQuantity() + quantity);
                warehouseStock.setUpdateDate(OffsetDateTime.now());
                warehouseStock.setUser(user);
                warehouseStock.setUserId(user.getId());
                warehouseStockRepository.save(warehouseStock);
            } else {
                warehouseStockRepository.save(WarehouseStock.builder()
                        .quantity(quantity)
                        .registrationDate(OffsetDateTime.now())
                        .product(product)
                        .productId(product.getId())
                        .warehouse(warehouse)
                        .warehouseId(warehouse.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .user(user).userId(user.getId())
                        .build());
            }
            iAudit.save(

                    "ADD_WAREHOUSE_STOCK",
                    "INGRESAN ("+quantity+") UNIDADES DE STOCK DE PRODUCTO DE INVENTARIO "+
                            iUtil.buildProductSku(product)+
                            " EN ALMACEN "+warehouse.getName()+".",
                    warehouse.getName(),user.getUsername());
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
    public ResponseSuccess out(Warehouse warehouse, Product product, Integer quantity, User user)
            throws InternalErrorExceptions, BadRequestExceptions {

        WarehouseStock warehouseStock;

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (product == null) {
            throw new BadRequestExceptions(Constants.ErrorProduct);
        }

        if (warehouse == null) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if (!Objects.equals(warehouse.getClientId(), user.getClientId())) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        try {
            warehouseStock = warehouseStockRepository.findByWarehouseIdAndProductId(warehouse.getId(),
                    product.getId());

            if (warehouseStock == null) {
                throw new BadRequestExceptions(Constants.ErrorWarehouseStock);
            }

            if (quantity > warehouseStock.getQuantity()) {
                throw new BadRequestExceptions(Constants.ErrorWarehouseStockLess);
            }

            warehouseStock.setQuantity(warehouseStock.getQuantity() - quantity);
            warehouseStockRepository.save(warehouseStock);
            iAudit.save(
                    "DELETE_WAREHOUSE_STOCK",
                    "SALIDA DE ("+quantity+") UNIDADES DE STOCK DE PRODUCTO DE INVENTARIO "+
                            iUtil.buildProductSku(product)+
                            " PARA ALMACEN "+warehouse.getName()+".",
                    warehouse.getName(),user.getUsername());
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
            String warehouse,
            String model,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<WarehouseStock> warehouseStockPage;
            UUID clientId;

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                warehouseStockPage = warehouseStockRepositoryCustom.searchForWarehouseStock(
                        clientId,
                        warehouse,
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
                    .map(warehouseStock -> {
                        String finalSku = iUtil.buildProductSku(warehouseStock.getProduct());
                        return WarehouseStockDTO.builder()
                                .id(warehouseStock.getId())
                                .user(warehouseStock.getUser().getUsername())
                                .quantity(warehouseStock.getQuantity())
                                .product(warehouseStock.getProduct().getName())
                                .productSku(finalSku)
                                .categoryProduct(warehouseStock.getProduct().getSubCategoryProduct().getCategoryProduct().getName())
                                .subCategoryProduct(warehouseStock.getProduct().getSubCategoryProduct().getName())
                                .model(warehouseStock.getProduct().getModel().getName())
                                .color(warehouseStock.getProduct().getColor().getName())
                                .size(warehouseStock.getProduct().getSize().getName())
                                .warehouse(warehouseStock.getWarehouse().getName())
                                .registrationDate(warehouseStock.getRegistrationDate())
                                .updateDate(warehouseStock.getUpdateDate())
                                .build();
                    }).toList();

            return new PageImpl<>(warehouseStockDTOs, warehouseStockPage.getPageable(),
                    warehouseStockPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<WarehouseStockDTO>> listWarehouse(String user,String warehouse,UUID supplierProductId) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<WarehouseStock> warehouseStocks;
            UUID clientId;
            UUID warehouseId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                if(warehouse != null){
                    warehouseId = warehouseRepository.findByNameAndStatusTrue(warehouse.toUpperCase()).getId();
                }else{
                    warehouseId = null;
                }
                if(warehouseId != null && supplierProductId != null){
                    warehouseStocks = warehouseStockRepository.findAllByWarehouseIdAndProductId(warehouseId,supplierProductId);
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
                    .map(warehouseStock -> {
                        String finalSku = iUtil.buildProductSku(warehouseStock.getProduct());
                        return WarehouseStockDTO.builder()
                                .id(warehouseStock.getId())
                                .user(warehouseStock.getUser().getUsername())
                                .quantity(warehouseStock.getQuantity())
                                .productSku(finalSku)
                                .product(warehouseStock.getProduct().getName())
                                .model(warehouseStock.getProduct().getModel().getName())
                                .color(warehouseStock.getProduct().getColor().getName())
                                .size(warehouseStock.getProduct().getSize().getName())
                                .warehouse(warehouseStock.getWarehouse().getName())
                                .registrationDate(warehouseStock.getRegistrationDate())
                                .updateDate(warehouseStock.getUpdateDate())
                                .build();
                    }).toList();
        });
    }
}
