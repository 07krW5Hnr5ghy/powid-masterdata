package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.CheckStockItemDTO;
import com.proyect.masterdata.dto.OrderItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderItem;
import com.proyect.masterdata.dto.response.ResponseCheckStockItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IOrderItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class OrderItemImpl implements IOrderItem {

    private final UserRepository userRepository;
    private final OrderItemRepository orderItemRepository;
    private final ProductRepository productRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final WarehouseStockRepository warehouseStockRepository;
    private final OrderingRepository orderingRepository;
    private final ProductPriceRepository productPriceRepository;
    private final ProductPictureRepository productPictureRepository;
    private final OrderItemRepositoryCustom orderItemRepositoryCustom;
    private final IAudit iAudit;
    private final DiscountRepository discountRepository;
    private final ColorRepository colorRepository;
    private final SizeRepository sizeRepository;
    @Override
    public ResponseSuccess save(Ordering ordering, RequestOrderItem requestOrderItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Product product;
        Discount discount;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            product = productRepository.findBySkuAndStatusTrue(requestOrderItem.getProduct());
            discount = discountRepository.findByName(requestOrderItem.getDiscount().toUpperCase());
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

        if(discount==null){
            throw new BadRequestExceptions(Constants.ErrorDiscount);
        }

        if(requestOrderItem.getQuantity()<1){
            throw new BadRequestExceptions(Constants.ErrorOrderItemZero);
        }

        try{
            OrderItem newOrderItem = orderItemRepository.save(OrderItem.builder()
                            .discount(discount)
                            .discountId(discount.getId())
                            .discountAmount(requestOrderItem.getDiscountAmount())
                            .ordering(ordering)
                            .orderId(ordering.getId())
                            .quantity(requestOrderItem.getQuantity())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .product(product)
                            .productId(product.getId())
                            .observations(requestOrderItem.getObservations().toUpperCase())
                            .status(true)
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .tokenUser(user.getUsername())
                    .build());
            iAudit.save("ADD_ORDER_ITEM","PRODUCTO "+newOrderItem.getProduct().getSku()+" DE PEDIDO "+newOrderItem.getOrderId()+" CON "+newOrderItem.getQuantity()+" UNIDADES AGREGADO.",newOrderItem.getOrderId().toString(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(Ordering ordering, RequestOrderItem requestOrderItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;
            Discount discount;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product = productRepository.findBySkuAndStatusTrue(requestOrderItem.getProduct());
                discount = discountRepository.findByName(requestOrderItem.getDiscount().toUpperCase());
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

            if(discount==null){
                throw new BadRequestExceptions(Constants.ErrorDiscount);
            }

            if(requestOrderItem.getQuantity()<1){
                throw new BadRequestExceptions(Constants.ErrorOrderItemZero);
            }

            try{
                OrderItem newOrderItem = orderItemRepository.save(OrderItem.builder()
                        .discount(discount)
                        .discountId(discount.getId())
                        .discountAmount(requestOrderItem.getDiscountAmount())
                        .ordering(ordering)
                        .orderId(ordering.getId())
                        .quantity(requestOrderItem.getQuantity())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .product(product)
                        .productId(product.getId())
                        .observations(requestOrderItem.getObservations().toUpperCase())
                        .status(true)
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .tokenUser(user.getUsername())
                        .build());
                iAudit.save("ADD_ORDER_ITEM","PRODUCTO "+newOrderItem.getProduct().getSku()+" DE PEDIDO "+newOrderItem.getOrderId()+" CON "+newOrderItem.getQuantity()+" UNIDADES AGREGADO.",newOrderItem.getOrderId().toString(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseCheckStockItem> checkStock(String productSku, Integer quantity, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
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
                                .key(warehouseStock.getWarehouse().getName())
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
                            .itemStockList(checkStockItemDTOList)
                            .build();
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(Long orderId, String productSku, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            OrderItem orderItem;
            Product product;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
                product = productRepository.findBySku(productSku.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }

            if(product == null){
                throw new InternalErrorExceptions(Constants.ErrorProduct);
            }else {
                orderItem = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
            }

            if(orderItem == null){
                throw new BadRequestExceptions(Constants.ErrorOrderItem);
            }

            try{
                orderItem.setStatus(false);
                orderItem.setUpdateDate(new Date(System.currentTimeMillis()));
                orderItem.setTokenUser(user.getUsername());
                orderItemRepository.save(orderItem);
                iAudit.save("DELETE_ORDER_ITEM","PRODUCTO "+orderItem.getProduct().getSku()+" DE PEDIDO "+orderItem.getOrderId()+" DESACTIVADO.",orderItem.getOrderId().toString(),user.getUsername());
                return ResponseDelete.builder()
                        .message(Constants.delete)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> add(Long orderId,RequestOrderItem requestOrderItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            Product product;
            OrderItem orderItem;
            Discount discount;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
                product = productRepository.findBySkuAndStatusTrue(requestOrderItem.getProduct().toUpperCase());
                discount = discountRepository.findByName(requestOrderItem.getDiscount().toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }

            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }else {
                orderItem  = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
            }

            if(orderItem != null ){
                throw new BadRequestExceptions(Constants.ErrorOrderItemExists);
            }

            if(discount==null){
                throw new BadRequestExceptions(Constants.ErrorDiscount);
            }

            try{
                OrderItem newOrderItem = orderItemRepository.save(OrderItem.builder()
                        .ordering(ordering)
                        .orderId(ordering.getId())
                        .status(true)
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .discount(discount)
                        .discountId(discount.getId())
                        .discountAmount(requestOrderItem.getDiscountAmount())
                        .observations(requestOrderItem.getObservations().toUpperCase())
                        .product(product)
                        .productId(product.getId())
                        .quantity(requestOrderItem.getQuantity())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .tokenUser(user.getUsername())
                        .build());
                iAudit.save("ADD_ORDER_ITEM","PRODUCTO "+newOrderItem.getProduct().getSku()+" DE PEDIDO "+newOrderItem.getOrderId()+" CON "+newOrderItem.getQuantity()+" UNIDADES.",newOrderItem.getOrderId().toString(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> update(Long orderId, RequestOrderItem requestOrderItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            Product product;
            OrderItem orderItem;
            Discount discount;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
                product = productRepository.findBySkuAndStatusTrue(requestOrderItem.getProduct().toUpperCase());
                discount = discountRepository.findByName(requestOrderItem.getDiscount().toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }

            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }else {
                orderItem  = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
            }

            if(orderItem == null ){
                throw new BadRequestExceptions(Constants.ErrorOrderItem);
            }

            if(discount==null){
                throw new BadRequestExceptions(Constants.ErrorDiscount);
            }

            try{
                orderItem.setQuantity(requestOrderItem.getQuantity());
                orderItem.setDiscount(discount);
                orderItem.setDiscountId(discount.getId());
                orderItem.setDiscountAmount(requestOrderItem.getDiscountAmount());
                orderItem.setUpdateDate(new Date(System.currentTimeMillis()));
                orderItem.setObservations(requestOrderItem.getObservations().toUpperCase());
                orderItemRepository.save(orderItem);
                iAudit.save("UPDATE_ORDER_ITEM","PRODUCTO "+orderItem.getProduct().getSku()+" DE PEDIDO "+orderItem.getOrderId()+" ACTUALIZADO.",orderItem.getOrderId().toString(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<OrderItemDTO>> listOrderItems(
            String user,
            Long orderId,
            String productSku,
            List<String> colors,
            List<String> sizes,
            Integer quantity,
            Double discount,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Long clientId;
            List<Long> colorIds;
            List<Long> sizeIds;
            Page<OrderItem> pageOrderItem;
            if(colors != null && !colors.isEmpty()){
                colorIds = colorRepository.findByNameIn(
                        colors.stream().map(String::toUpperCase).toList()
                ).stream().map(
                        Color::getId
                ).toList();
            }else{
                colorIds = new ArrayList<>();
            }
            if(sizes != null && !sizes.isEmpty()){
                sizeIds = sizeRepository.findByNameIn(
                        sizes.stream().map(String::toUpperCase).toList()
                ).stream().map(
                        Size::getId
                ).toList();
            }else{
                sizeIds = new ArrayList<>();
            }
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageOrderItem = orderItemRepositoryCustom.searchForOrderItem(
                        clientId,
                        orderId,
                        productSku,
                        colorIds,
                        sizeIds,
                        quantity,
                        discount,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(pageOrderItem.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<OrderItemDTO> orderItemDTOS = pageOrderItem.getContent().stream().map(orderItem -> {
                List<String> productPictures = productPictureRepository.findAlByClientIdAndProductId(clientId,orderItem.getProductId()).stream().map(ProductPicture::getProductPictureUrl).toList();
                ProductPrice productPrice = productPriceRepository.findByProductId(orderItem.getProductId());
                Double totalPrice = null;
                if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")){
                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-((productPrice.getUnitSalePrice() * orderItem.getQuantity())*(orderItem.getDiscountAmount()/100));
                }

                if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-(orderItem.getDiscountAmount());
                }

                if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity());
                }

                return OrderItemDTO.builder()
                        .unit(orderItem.getProduct().getUnit().getName())
                        .color(orderItem.getProduct().getColor().getName())
                        .size(orderItem.getProduct().getSize().getName())
                        .pictures(productPictures)
                        .category(orderItem.getProduct().getCategoryProduct().getName())
                        .sku(orderItem.getProduct().getSku())
                        .unitPrice(productPrice.getUnitSalePrice())
                        .discount(orderItem.getDiscount().getName())
                        .discountAmount(orderItem.getDiscountAmount())
                        .quantity(orderItem.getQuantity())
                        .orderId(orderItem.getOrderId())
                        .totalPrice(totalPrice)
                        .observations(orderItem.getObservations())
                        .registrationDate(orderItem.getRegistrationDate())
                        .updateDate(orderItem.getUpdateDate())
                        .build();
            }).toList();
            return new PageImpl<>(orderItemDTOS,pageOrderItem.getPageable(),pageOrderItem.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<OrderItemDTO>> listByOrder(String user, Long orderId) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Long clientId;
            List<OrderItem> orderItemList;
            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                orderItemList = orderItemRepository.findAllByClientIdAndOrderIdAndStatusTrue(clientId,orderId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(orderItemList.isEmpty()){
                return Collections.emptyList();
            }
            return orderItemList.stream().map(orderItem -> {
                ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(orderItem.getProductId());
                Double totalPrice = null;
                if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")){
                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-((productPrice.getUnitSalePrice() * orderItem.getQuantity())*(orderItem.getDiscountAmount()/100));
                }

                if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-(orderItem.getDiscountAmount());
                }

                if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity());
                }
                return OrderItemDTO.builder()
                    .unit(orderItem.getProduct().getUnit().getName())
                    .orderId(orderItem.getOrderId())
                    .color(orderItem.getProduct().getColor().getName())
                    .size(orderItem.getProduct().getSize().getName())
                    .sku(orderItem.getProduct().getSku())
                    .category(orderItem.getProduct().getCategoryProduct().getName())
                        .quantity(orderItem.getQuantity())
                    .unitPrice(productPrice.getUnitSalePrice())
                        .discountAmount(orderItem.getDiscountAmount())
                        .discount(orderItem.getDiscount().getName())
                        .totalPrice(totalPrice)
                    .build();
            }).toList();
        });
    }
}
