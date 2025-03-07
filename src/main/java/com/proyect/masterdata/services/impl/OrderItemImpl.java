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
import com.proyect.masterdata.services.IUtil;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class OrderItemImpl implements IOrderItem {

    private final UserRepository userRepository;
    private final OrderItemRepository orderItemRepository;
    private final ProductRepository productRepository;
    private final WarehouseStockRepository warehouseStockRepository;
    private final OrderingRepository orderingRepository;
    private final ProductPriceRepository productPriceRepository;
    private final ProductPictureRepository productPictureRepository;
    private final OrderItemRepositoryCustom orderItemRepositoryCustom;
    private final IAudit iAudit;
    private final DiscountRepository discountRepository;
    private final ColorRepository colorRepository;
    private final SizeRepository sizeRepository;
    private final CategoryProductRepository categoryProductRepository;
    private final IUtil iUtil;
    @Override
    public ResponseSuccess save(Ordering ordering, RequestOrderItem requestOrderItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Product product;
        Discount discount;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            product = productRepository.findByIdAndStatusTrue(requestOrderItem.getProductId());
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
                            .registrationDate(OffsetDateTime.now())
                            .updateDate(OffsetDateTime.now())
                            .user(user)
                            .userId(user.getId())
                    .build());
            iAudit.save("ADD_ORDER_ITEM","PRODUCTO "+iUtil.buildProductSku(newOrderItem.getProduct())+" DE PEDIDO "+newOrderItem.getOrderId()+" CON "+newOrderItem.getQuantity()+" UNIDADES AGREGADO.",newOrderItem.getOrderId().toString(),user.getUsername());
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
                product = productRepository.findByIdAndStatusTrue(requestOrderItem.getProductId());
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
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .user(user)
                        .userId(user.getId())
                        .build());
                iAudit.save("ADD_ORDER_ITEM","PRODUCTO "+iUtil.buildProductSku(newOrderItem.getProduct())+" DE PEDIDO "+newOrderItem.getOrderId()+" CON "+newOrderItem.getQuantity()+" UNIDADES AGREGADO.",newOrderItem.getOrderId().toString(),user.getUsername());
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
    public CompletableFuture<ResponseCheckStockItem> checkStock(UUID productId, Integer quantity, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;
            List<CheckStockItemDTO> checkStockItemDTOList = new ArrayList<>();

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product = productRepository.findByIdAndStatusTrue(productId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }

            try{
                Integer stockUnits = 0;
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

    @Transactional
    @Override
    public CompletableFuture<ResponseDelete> delete(UUID orderId, UUID productId, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            OrderItem orderItem;
            Product product;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
                product = productRepository.findByIdAndStatusTrue(productId);
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
                orderItem.setUpdateDate(OffsetDateTime.now());
                orderItem.setUser(user);
                orderItem.setUserId(user.getId());
                //orderItemRepository.save(orderItem);
                orderItemRepository.deleteOrderItemLogically(
                        orderId,
                        productId,
                        OffsetDateTime.now(),
                        user.getId()
                );
                iAudit.save("DELETE_ORDER_ITEM","PRODUCTO "+iUtil.buildProductSku(orderItem.getProduct())+" DE PEDIDO "+orderItem.getOrderId()+" DESACTIVADO.",orderItem.getOrderId().toString(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> add(UUID orderId,RequestOrderItem requestOrderItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            Product product;
            OrderItem orderItem;
            Discount discount;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
                product = productRepository.findByIdAndStatusTrue(requestOrderItem.getProductId());
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
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                                .user(user)
                                .userId(user.getId())
                        .build());
                iAudit.save("ADD_ORDER_ITEM","PRODUCTO "+iUtil.buildProductSku(newOrderItem.getProduct())+" DE PEDIDO "+newOrderItem.getOrderId()+" CON "+newOrderItem.getQuantity()+" UNIDADES.",newOrderItem.getOrderId().toString(),user.getUsername());
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

    @Transactional
    @Override
    public CompletableFuture<ResponseSuccess> update(UUID orderId, RequestOrderItem requestOrderItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            Product product;
            OrderItem orderItem;
            Discount discount;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
                product = productRepository.findByIdAndStatusTrue(requestOrderItem.getProductId());
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
                System.out.println("Imprimiendo orden Item");
                orderItem  = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
                System.out.println(orderItem);
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
                orderItem.setUpdateDate(OffsetDateTime.now());
                orderItem.setObservations(requestOrderItem.getObservations().toUpperCase());
                System.out.println(orderItem);
                orderItemRepository.updateOrderItemFields(
                        orderItem.getId(),
                        requestOrderItem.getQuantity(),
                        requestOrderItem.getDiscountAmount(),
                        requestOrderItem.getObservations().toUpperCase(),
                        discount.getId(),
                        OffsetDateTime.now());
                //orderItemRepository.save(orderItem);
                iAudit.save("UPDATE_ORDER_ITEM","PRODUCTO "+iUtil.buildProductSku(orderItem.getProduct())+" DE PEDIDO "+orderItem.getOrderId()+" ACTUALIZADO.",orderItem.getOrderId().toString(),user.getUsername());
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
            UUID orderId,
            String productSku,
            List<String> colors,
            List<String> sizes,
            List<String> categories,
            Integer quantity,
            Double discount,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            UUID clientId;
            List<UUID> colorIds;
            List<UUID> sizeIds;
            List<UUID> categoryIds;
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
            if(categories != null && !categories.isEmpty()){
                categoryIds = categoryProductRepository.findByNameIn(
                        categories.stream().map(String::toUpperCase).toList()
                ).stream().map(
                        CategoryProduct::getId
                ).toList();
            }else{
                categoryIds = new ArrayList<>();
            }
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageOrderItem = orderItemRepositoryCustom.searchForOrderItem(
                        clientId,
                        orderId,
                        productSku,
                        colorIds,
                        sizeIds,
                        categoryIds,
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
                        .subCategory(orderItem.getProduct().getSubCategoryProduct().getName())
                        .category(orderItem.getProduct().getSubCategoryProduct().getCategoryProduct().getName())
                        .sku(iUtil.buildProductSku(orderItem.getProduct()))
                        .unitPrice(productPrice.getUnitSalePrice())
                        .discount(orderItem.getDiscount().getName())
                        .discountAmount(orderItem.getDiscountAmount())
                        .quantity(orderItem.getQuantity())
                        .orderId(orderItem.getOrderId())
                        .model(orderItem.getProduct().getModel().getName())
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
    public CompletableFuture<List<OrderItemDTO>> listByOrder(String user, UUID orderId) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            UUID clientId;
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
                    .sku(iUtil.buildProductSku(orderItem.getProduct()))
                        .model(orderItem.getProduct().getModel().getName())
                    .category(orderItem.getProduct().getSubCategoryProduct().getCategoryProduct().getName())
                        .subCategory(orderItem.getProduct().getSubCategoryProduct().getName())
                        .quantity(orderItem.getQuantity())
                    .unitPrice(productPrice.getUnitSalePrice())
                        .discountAmount(orderItem.getDiscountAmount())
                        .discount(orderItem.getDiscount().getName())
                        .totalPrice(totalPrice)
                    .build();
            }).toList();
        });
    }

    @Override
    public CompletableFuture<List<OrderItemDTO>> listByOrderFalse(String user, UUID orderId) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            UUID clientId;
            List<OrderItem> orderItemList;
            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                orderItemList = orderItemRepository.findAllByClientIdAndOrderIdAndStatusFalse(clientId,orderId);
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
                        .sku(iUtil.buildProductSku(orderItem.getProduct()))
                        .model(orderItem.getProduct().getModel().getName())
                        .category(orderItem.getProduct().getSubCategoryProduct().getCategoryProduct().getName())
                        .subCategory(orderItem.getProduct().getSubCategoryProduct().getName())
                        .quantity(orderItem.getQuantity())
                        .unitPrice(productPrice.getUnitSalePrice())
                        .discountAmount(orderItem.getDiscountAmount())
                        .discount(orderItem.getDiscount().getName())
                        .totalPrice(totalPrice)
                        .build();
            }).toList();
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> activate(UUID orderId, UUID productId, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            OrderItem orderItem;
            Product product;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
                product = productRepository.findByIdAndStatusTrue(productId);
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
                orderItem.setStatus(true);
                orderItem.setUpdateDate(OffsetDateTime.now());
                orderItem.setUser(user);
                orderItem.setUserId(user.getId());
                orderItemRepository.save(orderItem);
                iAudit.save("ACTIVATE_ORDER_ITEM","PRODUCTO "+iUtil.buildProductSku(orderItem.getProduct())+" DE PEDIDO "+orderItem.getOrderId()+" ACTIVADO.",orderItem.getOrderId().toString(),user.getUsername());
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
}
