package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.CheckStockItemDTO;
import com.proyect.masterdata.dto.request.RequestItem;
import com.proyect.masterdata.dto.response.ResponseCheckStockItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class ItemImpl implements IItem {

    private final UserRepository userRepository;
    private final ItemRepository itemRepository;
    private final ProductRepository productRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final WarehouseStockRepository warehouseStockRepository;
    @Override
    public ResponseSuccess save(Ordering ordering, RequestItem requestItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Product product;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            product = productRepository.findBySkuAndStatusTrue(requestItem.getProductSku());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (product == null){
            throw new BadRequestExceptions(Constants.ErrorProduct);
        }

        try{
            itemRepository.save(Item.builder()
                            .discount(requestItem.getDiscount())
                            .ordering(ordering)
                            .orderId(ordering.getId())
                            .quantity(requestItem.getQuantity())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .product(product)
                            .productId(product.getId())
                            .observations(requestItem.getObservations())
                            .status(true)
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .tokenUser(user.getUsername())
                    .build());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseCheckStockItem checkStock(String productSku, Integer quantity, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Product product;
        List<SupplierProduct> supplierProductList;
        List<CheckStockItemDTO> checkStockItemDTOList = new ArrayList<>();

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            product = productRepository.findBySkuAndStatusTrue(productSku.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(product == null){
            throw new BadRequestExceptions(Constants.ErrorProduct);
        }else {
            supplierProductList = supplierProductRepository.findAllByProductIdAndStatusTrue(product.getId());
        }

        if(supplierProductList.isEmpty()){
            throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
        }

        try{

            Integer stockUnits = 0;

            for(SupplierProduct supplierProduct : supplierProductList){
                List<WarehouseStock> warehouseStockList = warehouseStockRepository.findAllBySupplierProductId(supplierProduct.getId());
                for(WarehouseStock warehouseStock : warehouseStockList){
                    stockUnits += warehouseStock.getQuantity();
                    checkStockItemDTOList.add(CheckStockItemDTO.builder()
                                    .stockQuantity(warehouseStock.getQuantity())
                                    .warehouse(warehouseStock.getWarehouse().getName())
                            .build());
                }
            }

            if(stockUnits >= quantity){
                return ResponseCheckStockItem.builder()
                        .itemStockList(checkStockItemDTOList)
                        .pendingStock(false)
                        .pendingQuantity(0)
                        .build();
            }else {
                return ResponseCheckStockItem.builder()
                        .pendingQuantity(quantity-stockUnits)
                        .pendingStock(true)
                        .build();
            }
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

    }

    @Override
    public ResponseDelete delete(Long orderId, Long itemId, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Item item;
        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            item = itemRepository.findByIdAndOrderId(itemId,orderId);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(item == null){
            throw new InternalErrorExceptions(Constants.ErrorItem);
        }

        try{
            item.setStatus(false);
            item.setUpdateDate(new Date(System.currentTimeMillis()));
            item.setTokenUser(user.getUsername());
            itemRepository.save(item);
            return ResponseDelete.builder()
                    .message(Constants.delete)
                    .code(200)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
