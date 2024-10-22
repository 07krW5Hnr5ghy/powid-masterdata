package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.*;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IStats;
import com.proyect.masterdata.services.IUtil;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Log4j2
public class StatsImpl implements IStats {
    private final UserRepository userRepository;
    private final OrderStateRepository orderStateRepository;
    private final OrderingRepository orderingRepository;
    private final OrderItemRepository orderItemRepository;
    private final ProductPriceRepository productPriceRepository;
    private final IUtil iUtil;
    private final BrandRepository brandRepository;
    private final CategoryProductRepository categoryProductRepository;
    private final SaleChannelRepository saleChannelRepository;
    private final StatsRepository statsRepository;

    @Autowired
    private final Executor asyncExecutor;
    @Override
    public CompletableFuture<StatsCardDTO> listCardStats(
            Date registrationStartDate,
            Date registrationEndDate,
            String orderStateName,
            String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderState orderState;
            List<Ordering> orderingListByDate;
            List<Ordering> orderingListByDateAndStatus;
            Date utcRegistrationDateStart;
            Date utcRegistrationDateEnd;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                orderState = orderStateRepository.findByName(orderStateName);
                utcRegistrationDateStart = iUtil.setToUTCStartOfDay(registrationStartDate);
                utcRegistrationDateEnd = iUtil.setToUTCEndOfDay(registrationEndDate);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                orderingListByDate = orderingRepository.findByClientIdAndRegistrationDateBetween(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd);
            }
            if (orderState==null){
                orderingListByDateAndStatus = orderingRepository.findByClientIdAndRegistrationDateBetween(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd);
            }else {
                orderingListByDateAndStatus = orderingRepository.findByClientIdAndRegistrationDateBetweenAndOrderStateId(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd,
                        orderState.getId());
            }
            try{
                int totalOrdersByDate;
                int totalOrdersByDateAndStatus = orderingListByDateAndStatus.size();
                if(orderingListByDate.isEmpty()){
                    totalOrdersByDate = 0;
                }else{
                    totalOrdersByDate = orderingListByDate.size();
                }
                String state;
                if(orderState!=null){
                    state = orderState.getName();
                }else{
                    state = "TODOS";
                }
                double totalSales = 0.00;
                double totalDeliveryAmount = 0.00;
                int totalProducts = 0;
                for(Ordering ordering:orderingListByDate){
                    double totalSaleByOrder = 0.00;
                    Integer totalProductsByOrder = 0;
                    List<OrderItem> orderItems = orderItemRepository.findAllByOrderIdAndStatusTrue(ordering.getId());
                    for(OrderItem orderItem:orderItems){
                        double totalPrice = 0.00;
                        ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(orderItem.getProductId());
                        if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")) {
                            totalPrice += (productPrice.getUnitSalePrice() * orderItem.getQuantity()) - ((productPrice.getUnitSalePrice() * orderItem.getQuantity()) * (orderItem.getDiscountAmount() / 100));
                        }
                        if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                            totalPrice += (productPrice.getUnitSalePrice() * orderItem.getQuantity()) - orderItem.getDiscountAmount();
                        }
                        if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                            totalPrice += (productPrice.getUnitSalePrice() * orderItem.getQuantity());
                        }
                        totalSaleByOrder+=totalPrice;
                        totalProductsByOrder += orderItem.getQuantity();
                    }
                    totalSales += totalSaleByOrder;
                    totalDeliveryAmount += ordering.getDeliveryAmount();
                    totalProducts += totalProductsByOrder;
                }
                double totalSalesByStatus = 0.00;
                double totalDeliveryAmountByStatus = 0.00;
                int totalProductsByStatus = 0;
                for(Ordering ordering:orderingListByDateAndStatus){
                    double totalSaleByOrder = 0.00;
                    Integer totalProductsByOrder = 0;
                    List<OrderItem> orderItems = orderItemRepository.findAllByOrderIdAndStatusTrue(ordering.getId());
                    for(OrderItem orderItem:orderItems){
                        double totalPrice = 0.00;
                        ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(orderItem.getProductId());
                        if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")) {
                            totalPrice += (productPrice.getUnitSalePrice() * orderItem.getQuantity()) - ((productPrice.getUnitSalePrice() * orderItem.getQuantity()) * (orderItem.getDiscountAmount() / 100));
                        }
                        if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                            totalPrice += (productPrice.getUnitSalePrice() * orderItem.getQuantity()) - orderItem.getDiscountAmount();
                        }
                        if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                            totalPrice += (productPrice.getUnitSalePrice() * orderItem.getQuantity());
                        }
                        totalSaleByOrder +=  totalPrice;
                        totalProductsByOrder += orderItem.getQuantity();
                    }
                    totalSalesByStatus += totalSaleByOrder;
                    totalDeliveryAmountByStatus += ordering.getDeliveryAmount();
                    totalProductsByStatus += totalProductsByOrder;
                }

                if(totalSalesByStatus <= 0.00 && totalProductsByStatus < 1){
                    return StatsCardDTO.builder()
                            .totalOrders(totalOrdersByDateAndStatus)
                            .totalSalesByStatus(BigDecimal.valueOf(totalSalesByStatus).setScale(2, RoundingMode.HALF_EVEN))
                            .orderStatus(state)
                            .totalDeliveryAmountOrders(BigDecimal.valueOf(0.00).setScale(2, RoundingMode.HALF_EVEN))
                            .totalProducts(totalProductsByStatus)
                            .totalSalesByRangeDate(BigDecimal.valueOf(0.00).setScale(2,RoundingMode.HALF_EVEN))
                            .totalOrdersByRangeDate(0)
                            .averageSaleProduct(BigDecimal.valueOf(0.00).setScale(2,RoundingMode.HALF_EVEN))
                            .averageTicket(BigDecimal.valueOf(0.00).setScale(2, RoundingMode.HALF_EVEN))
                            .percentageOfOrders(BigDecimal.valueOf(0.00).setScale(2, RoundingMode.HALF_EVEN))
                            .percentageOfSales(BigDecimal.valueOf(0.00).setScale(2, RoundingMode.HALF_EVEN))
                            .build();
                }

                double averageSaleProduct;
                if(totalSalesByStatus > 0.00 && totalProductsByStatus > 0){
                    averageSaleProduct = totalSalesByStatus/totalProductsByStatus;
                }else{
                    averageSaleProduct = 0.00;
                }

                BigDecimal totalSalesByDateAndStatusBD = BigDecimal.valueOf(totalSalesByStatus);
                BigDecimal totalSalesByDateBD = BigDecimal.valueOf(totalSales);
                BigDecimal totalOrdersByDateAndStatusBD = BigDecimal.valueOf(totalOrdersByDateAndStatus);
                BigDecimal totalOrdersByDateBD = BigDecimal.valueOf(totalOrdersByDate);

                BigDecimal percentageOfSales = totalSalesByDateAndStatusBD
                        .divide(totalSalesByDateBD, 2, RoundingMode.HALF_EVEN) // Divide with 2 decimal places
                        .multiply(BigDecimal.valueOf(100L)).setScale(2,RoundingMode.HALF_EVEN);

                BigDecimal percentageOfOrders = totalOrdersByDateAndStatusBD
                        .divide(totalOrdersByDateBD, 2, RoundingMode.HALF_EVEN) // Divide with 2 decimal places
                        .multiply(BigDecimal.valueOf(100L)).setScale(2,RoundingMode.HALF_EVEN);

                return StatsCardDTO.builder()
                        .totalOrders(totalOrdersByDateAndStatus)
                        .totalSalesByRangeDate(BigDecimal.valueOf(totalSales).setScale(2,RoundingMode.HALF_EVEN))
                        .totalOrdersByRangeDate(totalOrdersByDate)
                        .totalSalesByStatus(BigDecimal.valueOf(totalSalesByStatus).setScale(2, RoundingMode.HALF_EVEN))
                        .orderStatus(state)
                        .totalDeliveryAmountOrders(BigDecimal.valueOf(totalDeliveryAmountByStatus).setScale(2, RoundingMode.HALF_EVEN))
                        .totalProducts(totalProductsByStatus)
                        .averageSaleProduct(BigDecimal.valueOf(averageSaleProduct).setScale(2,RoundingMode.HALF_EVEN))
                        .averageTicket(BigDecimal.valueOf(totalProductsByStatus/totalOrdersByDateAndStatus).setScale(2,RoundingMode.HALF_EVEN))
                        .percentageOfOrders(percentageOfOrders)
                        .percentageOfSales(percentageOfSales)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<DailySaleSummaryDTO>> listDailySales(Date registrationStartDate, Date registrationEndDate, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            List<Ordering> orderingListByDate;
            Date utcRegistrationDateStart;
            Date utcRegistrationDateEnd;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                utcRegistrationDateStart = iUtil.setToUTCStartOfDay(registrationStartDate);
                utcRegistrationDateEnd = iUtil.setToUTCEndOfDay(registrationEndDate);
                System.out.println(utcRegistrationDateStart);
                System.out.println(utcRegistrationDateEnd);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                orderingListByDate = orderingRepository.findByClientIdAndRegistrationDateBetween(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd);
            }
            try{
                List<DailySaleSummaryDTO> dailySaleSummaryDTOS = new ArrayList<>();
                List<DailySaleSummaryDTO> orderDates = orderingRepository.findAllOrdersByDate(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd).stream().map(result -> DailySaleSummaryDTO.builder()
                                    .date((Date) result[0])
                                    .orderState("TODOS")
                                    .totalOrders(((Long) result[1]).intValue())
                                    .build()
                ).toList();
                double totalSales = 0.00;
                for(DailySaleSummaryDTO dailySaleSummaryDTO:orderDates){
                    double dailyTotalSales = 0.00;
                    for(Ordering ordering:orderingListByDate){
                        double orderTotalSales = 0.00;
                        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                        String formattedDate = sdf.format(ordering.getRegistrationDate());
                        if(dailySaleSummaryDTO.getDate().toString().equals(formattedDate)){
                            List<OrderItem> orderItemList = orderItemRepository.findAllByClientIdAndOrderIdAndStatusTrue(
                                    user.getClientId(),
                                    ordering.getId()
                            );
                            for(OrderItem orderItem:orderItemList){
                                ProductPrice productPrice = productPriceRepository.findByProductId(orderItem.getProductId());
                                double totalPrice = 0.00;
                                if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-((productPrice.getUnitSalePrice() * orderItem.getQuantity())*(orderItem.getDiscountAmount()/100));
                                }

                                if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-(orderItem.getDiscountAmount());
                                }

                                if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity());
                                }
                                orderTotalSales+=totalPrice;
                            }
                        }
                        dailyTotalSales+=orderTotalSales;
                    }
                    totalSales+=dailyTotalSales;
                    dailySaleSummaryDTO.setTotalSalePerDay(BigDecimal.valueOf(dailyTotalSales).setScale(2,RoundingMode.HALF_EVEN));
                    dailySaleSummaryDTOS.add(dailySaleSummaryDTO);
                }
                return dailySaleSummaryDTOS;
            }catch (RuntimeException e){
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<DailySaleSummaryDTO>> listDailySalesByStatus(Date registrationStartDate, Date registrationEndDate, String status, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderState orderState;
            List<Ordering> orderingListByDateAndStatus;
            Date utcRegistrationDateStart;
            Date utcRegistrationDateEnd;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                orderState = orderStateRepository.findByNameAndStatusTrue(status.toUpperCase());
                utcRegistrationDateStart = iUtil.setToUTCStartOfDay(registrationStartDate);
                utcRegistrationDateEnd = iUtil.setToUTCEndOfDay(registrationEndDate);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(orderState==null){
                throw new BadRequestExceptions(Constants.ErrorOrderState);
            }else{
                orderingListByDateAndStatus = orderingRepository.findByClientIdAndRegistrationDateBetweenAndOrderStateId(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd,
                        orderState.getId()
                );
            }
            try{
                List<DailySaleSummaryDTO> dailySaleSummaryDTOS = new ArrayList<>();
                List<DailySaleSummaryDTO> orderDates = orderingRepository.findOrderCountByDateAndStatus(
                        user.getClientId(),
                        orderState.getId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd).stream().map(result -> DailySaleSummaryDTO.builder()
                            .orderState(orderState.getName())
                            .date((Date) result[0])
                            .totalOrders(((Long) result[1]).intValue())
                            .build()
                ).toList();

                for(DailySaleSummaryDTO dailySaleSummaryDTO:orderDates){
                    double dailyTotalSales = 0.00;
                    for(Ordering ordering:orderingListByDateAndStatus){
                        double orderTotalSales = 0.00;
                        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                        String formattedDate = sdf.format(ordering.getRegistrationDate());
                        if(dailySaleSummaryDTO.getDate().toString().equals(formattedDate)){
                            List<OrderItem> orderItemList = orderItemRepository.findAllByClientIdAndOrderIdAndStatusTrue(
                                    user.getClientId(),
                                    ordering.getId()
                            );
                            for(OrderItem orderItem:orderItemList){
                                ProductPrice productPrice = productPriceRepository.findByProductId(orderItem.getProductId());
                                double totalPrice = 0.00;
                                if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-((productPrice.getUnitSalePrice() * orderItem.getQuantity())*(orderItem.getDiscountAmount()/100));
                                }

                                if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-(orderItem.getDiscountAmount());
                                }

                                if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity());
                                }
                                orderTotalSales+=totalPrice;
                            }
                        }
                        dailyTotalSales+=orderTotalSales;
                    }
                    dailySaleSummaryDTO.setTotalSalePerDay(BigDecimal.valueOf(dailyTotalSales).setScale(2,RoundingMode.HALF_EVEN));
                    dailySaleSummaryDTOS.add(dailySaleSummaryDTO);
                }
                return dailySaleSummaryDTOS;
            }catch (RuntimeException e){
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<SellerSalesDTO>> listSellerSales(
            Date registrationStartDate,
            Date registrationEndDate,
            String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            List<SellerSalesDTO> orderingList;
            Date utcRegistrationDateStart;
            Date utcRegistrationDateEnd;
            List<Ordering> orderingListByDate;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                utcRegistrationDateStart = iUtil.setToUTCStartOfDay(registrationStartDate);
                utcRegistrationDateEnd = iUtil.setToUTCEndOfDay(registrationEndDate);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            try{
                orderingList = orderingRepository.findByClientIdAndRegistrationDateBetweenCountSeller(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd
                ).stream().map(result -> SellerSalesDTO.builder()
                        .seller(result[0].toString())
                        .orderCount((long) result[1])
                        .build()
                ).toList();
                orderingListByDate = orderingRepository.findByClientIdAndRegistrationDateBetween(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd);
                for(SellerSalesDTO sellerSalesDto:orderingList){
                    double sellerTotalSales = 0.00;
                    int sellerTotalProducts = 0;
                    for(Ordering ordering:orderingListByDate){
                        double orderTotalSales = 0.00;
                        int orderTotalProducts = 0;
                        if(Objects.equals(sellerSalesDto.getSeller(), ordering.getSeller())){
                            List<OrderItem> orderItemList = orderItemRepository.findAllByClientIdAndOrderIdAndStatusTrue(
                                    user.getClientId(),
                                    ordering.getId()
                            );
                            for(OrderItem orderItem:orderItemList){
                                ProductPrice productPrice = productPriceRepository.findByProductId(orderItem.getProductId());
                                double totalPrice = 0.00;
                                if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-((productPrice.getUnitSalePrice() * orderItem.getQuantity())*(orderItem.getDiscountAmount()/100));
                                }

                                if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-(orderItem.getDiscountAmount());
                                }

                                if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity());
                                }
                                orderTotalSales+=totalPrice;
                                orderTotalProducts += orderItem.getQuantity();
                            }
                            sellerTotalSales+=orderTotalSales;
                            sellerTotalProducts+=orderTotalProducts;
                        }
                    }
                    sellerSalesDto.setTotalSales(BigDecimal.valueOf(sellerTotalSales).setScale(2,RoundingMode.HALF_EVEN));
                    sellerSalesDto.setProductCount(sellerTotalProducts);
                    sellerSalesDto.setAverageTicket(BigDecimal.valueOf(sellerTotalProducts/sellerSalesDto.getOrderCount()).setScale(2,RoundingMode.HALF_EVEN));
                }
                return orderingList;
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.ErrorUser);
            }
        });
    }

    @Override
    public CompletableFuture<List<SalesBrandDTO>> listSalesBrand(Date registrationStartDate, Date registrationEndDate, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            List<DailySaleSummaryDTO> orderingList;
            List<Ordering> orderingListByDate;
            Date utcRegistrationDateStart;
            Date utcRegistrationDateEnd;
            List<Brand> brands;
            try{
                user =  userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                utcRegistrationDateStart = iUtil.setToUTCStartOfDay(registrationStartDate);
                utcRegistrationDateEnd = iUtil.setToUTCEndOfDay(registrationEndDate);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                brands = brandRepository.findAllByClientIdAndStatusTrue(user.getClientId());
                orderingList = orderingRepository.findAllOrdersByDate(
                       user.getClientId(),
                       utcRegistrationDateStart,
                       utcRegistrationDateEnd
                   ).stream().map(result -> DailySaleSummaryDTO.builder()
                           .date((Date) result[0])
                           .build()
                ).toList();
                orderingListByDate = orderingRepository.findByClientIdAndRegistrationDateBetween(
                           user.getClientId(),
                           utcRegistrationDateStart,
                           utcRegistrationDateEnd
                );
            }
            try{
                List<SalesBrandDTO> salesBrandDTOS = new ArrayList<>();
                for(Brand brand:brands){
                    for(DailySaleSummaryDTO dailySaleSummaryDTO:orderingList){
                        SalesBrandDTO salesBrandDTOCurrent = SalesBrandDTO.builder().build();
                        salesBrandDTOCurrent.setBrand(brand.getName());
                        salesBrandDTOCurrent.setDate(dailySaleSummaryDTO.getDate());
                        salesBrandDTOS.add(salesBrandDTOCurrent);
                    }
                }
                for(SalesBrandDTO salesBrandDTO:salesBrandDTOS){
                    double totalSalesByDateAndBrand = 0.00;
                    int totalProductsByDateAndBrand = 0;
                    int totalOrdersByDateAndBrand = 0;
                    double totalSalesByDateAndBrandDelivered = 0.00;
                    for(Ordering ordering:orderingListByDate){
                        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                        String formattedDate = sdf.format(ordering.getRegistrationDate());
                        if(Objects.equals(salesBrandDTO.getDate().toString(), formattedDate)){
                            List<OrderItem> orderItemList = orderItemRepository.findAllByClientIdAndOrderIdAndStatusTrue(
                                    user.getClientId(),
                                    ordering.getId()
                            );
                            double orderTotalSales = 0.00;
                            double orderTotalSalesDelivered = 0.00;
                            int orderTotalProducts = 0;
                            boolean brandOrderFlag = false;
                            for(OrderItem orderItem:orderItemList){
                                if(Objects.equals(orderItem.getProduct().getModel().getBrand().getName(), salesBrandDTO.getBrand())){
                                    brandOrderFlag = true;
                                    ProductPrice productPrice = productPriceRepository.findByProductId(orderItem.getProductId());
                                    double totalPrice = 0.00;
                                    if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")){
                                        totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-((productPrice.getUnitSalePrice() * orderItem.getQuantity())*(orderItem.getDiscountAmount()/100));
                                    }

                                    if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                                        totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-(orderItem.getDiscountAmount());
                                    }

                                    if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                                        totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity());
                                    }

                                    if(Objects.equals(ordering.getOrderState().getName(), "ENTREGADO")){
                                        orderTotalSalesDelivered += totalPrice;
                                    }
                                    orderTotalSales+=totalPrice;
                                    orderTotalProducts += orderItem.getQuantity();
                                }
                            }
                            if(brandOrderFlag){
                                totalOrdersByDateAndBrand++;
                            }
                            totalSalesByDateAndBrand += orderTotalSales;
                            totalProductsByDateAndBrand += orderTotalProducts;
                            totalSalesByDateAndBrandDelivered += orderTotalSalesDelivered;
                        }
                    }
                    salesBrandDTO.setTotalSales(BigDecimal.valueOf(totalSalesByDateAndBrand));
                    salesBrandDTO.setTotalProducts(totalProductsByDateAndBrand);
                    salesBrandDTO.setTotalOrders(totalOrdersByDateAndBrand);
                    if(totalProductsByDateAndBrand < 1 || totalOrdersByDateAndBrand < 1){
                        salesBrandDTO.setAverageTicket(BigDecimal.valueOf(0.00).setScale(2,RoundingMode.HALF_EVEN));
                    }else{
                        salesBrandDTO.setAverageTicket(BigDecimal.valueOf(totalProductsByDateAndBrand/totalOrdersByDateAndBrand).setScale(2,RoundingMode.HALF_EVEN));
                    }
                    salesBrandDTO.setDeliveredAmount(BigDecimal.valueOf(totalSalesByDateAndBrandDelivered).setScale(2,RoundingMode.HALF_EVEN));
                }
                return salesBrandDTOS;
            } catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<SalesStatusDTO>> listSalesStatus(Date registrationStartDate, Date registrationEndDate, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            List<SalesStatusDTO> salesStatusDTOS;
            Date utcRegistrationDateStart;
            Date utcRegistrationDateEnd;
            List<Ordering> orderingList;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                utcRegistrationDateStart = iUtil.setToUTCStartOfDay(registrationStartDate);
                utcRegistrationDateEnd = iUtil.setToUTCEndOfDay(registrationEndDate);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                salesStatusDTOS = orderingRepository.findByClientIdAndStateRegistrationDateBetween(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd
                ).stream().map(result -> SalesStatusDTO.builder()
                                .status((String) result[0])
                                .totalOrders((Long) result[1])
                                .build()
                        ).toList();
                orderingList = orderingRepository.findByClientIdAndRegistrationDateBetween(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd
                );
            }
            try{
                for(SalesStatusDTO salesStatusDTO:salesStatusDTOS){
                    double totalSalesByStatus = 0.00;
                    int totalProductsByStatus = 0;
                    for(Ordering ordering:orderingList){
                        if(Objects.equals(salesStatusDTO.getStatus(), ordering.getOrderState().getName())){
                            List<OrderItem> orderItemList = orderItemRepository.findAllByClientIdAndOrderIdAndStatusTrue(
                                    user.getClientId(),
                                    ordering.getId()
                            );
                            double orderTotalSales = 0.00;
                            int orderTotalProducts = 0;
                            for(OrderItem orderItem:orderItemList){
                                ProductPrice productPrice = productPriceRepository.findByProductId(orderItem.getProductId());
                                double totalPrice = 0.00;
                                if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-((productPrice.getUnitSalePrice() * orderItem.getQuantity())*(orderItem.getDiscountAmount()/100));
                                }

                                if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-(orderItem.getDiscountAmount());
                                }

                                if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity());
                                }
                                orderTotalSales+=totalPrice;
                                orderTotalProducts += orderItem.getQuantity();
                            }
                            totalSalesByStatus += orderTotalSales;
                            totalProductsByStatus += orderTotalProducts;
                        }
                    }
                    salesStatusDTO.setTotalSales(BigDecimal.valueOf(totalSalesByStatus).setScale(2,RoundingMode.HALF_EVEN));
                    salesStatusDTO.setTotalProducts(totalProductsByStatus);
                    if(totalProductsByStatus < 1 || salesStatusDTO.getTotalOrders() < 1){
                        salesStatusDTO.setAverageTicket(BigDecimal.valueOf(0.00).setScale(2,RoundingMode.HALF_EVEN));
                    }else{
                        salesStatusDTO.setAverageTicket(BigDecimal.valueOf(totalProductsByStatus/salesStatusDTO.getTotalOrders()).setScale(2,RoundingMode.HALF_EVEN));
                    }
                }
                return salesStatusDTOS;
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<SalesChannelDTO>> listSalesChannel(
            Date registrationStartDate,
            Date registrationEndDate,
            String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Date utcRegistrationDateStart;
            Date utcRegistrationDateEnd;
            List<Ordering> orderingList;
            List<SalesChannelDTO> salesChannelDTOS;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username);
                utcRegistrationDateStart = iUtil.setToUTCStartOfDay(registrationStartDate);
                utcRegistrationDateEnd = iUtil.setToUTCEndOfDay(registrationEndDate);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                salesChannelDTOS = orderingRepository.findByClientIdAndSaleChannelRegistrationDateBetween(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd
                ).stream().map(result -> SalesChannelDTO.builder()
                        .saleChannel((String) result[0])
                        .totalOrders((Long) result[1])
                        .build()
                ).toList();
                orderingList = orderingRepository.findByClientIdAndRegistrationDateBetween(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd
                );
            }
            try{
                for(SalesChannelDTO salesChannelDTO:salesChannelDTOS){
                    double totalSalesByChannel = 0.00;
                    int totalProductsByChannel = 0;
                    for(Ordering ordering:orderingList){
                        if(Objects.equals(salesChannelDTO.getSaleChannel(), ordering.getSaleChannel().getName())){
                            List<OrderItem> orderItemList = orderItemRepository.findAllByClientIdAndOrderIdAndStatusTrue(
                                    user.getClientId(),
                                    ordering.getId()
                            );
                            double orderTotalSales = 0.00;
                            int orderTotalProducts = 0;
                            for(OrderItem orderItem:orderItemList){
                                ProductPrice productPrice = productPriceRepository.findByProductId(orderItem.getProductId());
                                double totalPrice = 0.00;
                                if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-((productPrice.getUnitSalePrice() * orderItem.getQuantity())*(orderItem.getDiscountAmount()/100));
                                }

                                if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-(orderItem.getDiscountAmount());
                                }

                                if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity());
                                }
                                orderTotalSales+=totalPrice;
                                orderTotalProducts += orderItem.getQuantity();
                            }
                            totalSalesByChannel += orderTotalSales;
                            totalProductsByChannel += orderTotalProducts;
                        }
                    }
                    salesChannelDTO.setTotalSales(BigDecimal.valueOf(totalSalesByChannel).setScale(2,RoundingMode.HALF_EVEN));
                    salesChannelDTO.setTotalProducts(totalProductsByChannel);
                    if(totalProductsByChannel < 1 || salesChannelDTO.getTotalOrders() < 1){
                        salesChannelDTO.setAverageTicket(BigDecimal.valueOf(0.00).setScale(2,RoundingMode.HALF_EVEN));
                    }else{
                        salesChannelDTO.setAverageTicket(BigDecimal.valueOf(totalProductsByChannel/salesChannelDTO.getTotalOrders()).setScale(2,RoundingMode.HALF_EVEN));
                    }
                }
                return salesChannelDTOS;
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<SalesCategoryDTO>> listCategories(
            Date registrationStartDate,
            Date registrationEndDate,
            String username,
            Integer page,
            Integer size
    ) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            List<Ordering> orderingList;
            Date utcRegistrationDateStart;
            Date utcRegistrationDateEnd;
            List<SalesCategoryRawDTO> salesCategoryRawDTOS = new ArrayList<>();
            List<CategoryProduct> categoryProducts;
            List<SaleChannel> saleChannels;

            try {
                // Fetch user by username
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                if (user == null) {
                    throw new BadRequestExceptions(Constants.ErrorUser);
                }
                categoryProducts = categoryProductRepository.findAll();
                saleChannels = saleChannelRepository.findAll();

                // Convert registration dates to UTC
                utcRegistrationDateStart = iUtil.setToUTCStartOfDay(registrationStartDate);
                utcRegistrationDateEnd = iUtil.setToUTCEndOfDay(registrationEndDate);

                orderItemRepository.findOrderItemsByDateRangeAndClientId(
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd,
                        user.getClientId()
                ).forEach(item->{
                    salesCategoryRawDTOS.add(
                            SalesCategoryRawDTO.builder()
                                    .orderId((Long) item[0])
                                    .registrationDate((Date) item[1])
                                    .orderDiscountAmount((Double) item[3])
                                    .orderDiscountName((String) item[4])
                                    .saleChannelName((String) item[5])
                                    .quantity((Integer) item[7])
                                    .orderItemDiscountAmount((Double) item[8])
                                    .orderItemDiscountName((String) item[9])
                                    .categoryName((String) item[10])
                                    .unitSalePrice((Double) item[11])
                                    .orderItemStatus((Boolean) item[12])
                                    .build()
                    );
                });
            } catch (RuntimeException e) {
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            try {
                List<SalesCategoryDTO> salesCategoryDTOS = new ArrayList<>();
                // Initialize DTO for each category and sale channel combination
                for (CategoryProduct categoryProduct : categoryProducts) {
                    for (SaleChannel saleChannel : saleChannels) {
                        SalesCategoryDTO salesCategoryDTOCurrent = SalesCategoryDTO.builder()
                                .category(categoryProduct.getName())
                                .saleChannel(saleChannel.getName())
                                .build();
                        salesCategoryDTOS.add(salesCategoryDTOCurrent);
                    }
                }

                for (SalesCategoryDTO salesCategoryDTO : salesCategoryDTOS) {
                    double totalSalesByCategory = 0.00;
                    int totalProductsByCategory = 0;
                    int totalOrdersByCategory = 0;

                    for (SalesCategoryRawDTO salesCategoryRawDTO : salesCategoryRawDTOS) {
                        if (Objects.equals(salesCategoryDTO.getSaleChannel(), salesCategoryRawDTO.getSaleChannelName())) {

                            double orderTotalSales = 0.00;
                            int orderTotalProducts = 0;
                            boolean orderFlag = false;
                            if (Objects.equals(salesCategoryDTO.getCategory(), salesCategoryRawDTO.getCategoryName())) {
                                if(salesCategoryRawDTO.getOrderItemStatus()){
                                    orderFlag = true;
                                    double orderItemPrice = 0.00;
                                    if (salesCategoryRawDTO.getOrderItemDiscountName() != null) {
                                        if (salesCategoryRawDTO.getOrderItemDiscountName().equals("MONTO")) {
                                            orderItemPrice = (salesCategoryRawDTO.getUnitSalePrice() * salesCategoryRawDTO.getQuantity()) - salesCategoryRawDTO.getOrderItemDiscountAmount();
                                        } else if (salesCategoryRawDTO.getOrderItemDiscountName().equals("PORCENTAJE")) {
                                            double discount = (salesCategoryRawDTO.getUnitSalePrice() * salesCategoryRawDTO.getQuantity()) * (salesCategoryRawDTO.getOrderItemDiscountAmount() / 100);
                                            orderItemPrice = (salesCategoryRawDTO.getUnitSalePrice() * salesCategoryRawDTO.getQuantity()) - discount;
                                        } else if (salesCategoryRawDTO.getOrderItemDiscountName().equals("NO APLICA")) {
                                            orderItemPrice = salesCategoryRawDTO.getUnitSalePrice() * salesCategoryRawDTO.getQuantity();
                                        }
                                    } else {
                                        // Fallback in case orderItemDiscountName is null or doesn't match any condition
                                        orderItemPrice = salesCategoryRawDTO.getUnitSalePrice() * salesCategoryRawDTO.getQuantity();
                                    }
                                    orderTotalSales += orderItemPrice;
                                    orderTotalProducts += salesCategoryRawDTO.getQuantity();
                                }
                            }

                            if (orderFlag) {
                                totalOrdersByCategory++;
                            }
                            totalSalesByCategory += orderTotalSales;
                            totalProductsByCategory += orderTotalProducts;
                        }
                    }

                    // Set aggregated values to DTO
                    salesCategoryDTO.setTotalSales(BigDecimal.valueOf(totalSalesByCategory).setScale(2, RoundingMode.HALF_EVEN));
                    salesCategoryDTO.setTotalProducts(totalProductsByCategory);
                    salesCategoryDTO.setTotalOrders(totalOrdersByCategory);

                    // Calculate average ticket
                    if (totalProductsByCategory < 1 || salesCategoryDTO.getTotalOrders() < 1) {
                        salesCategoryDTO.setAverageTicket(BigDecimal.valueOf(0.00).setScale(2, RoundingMode.HALF_EVEN));
                    } else {
                        salesCategoryDTO.setAverageTicket(BigDecimal.valueOf((double) totalProductsByCategory / totalOrdersByCategory).setScale(2, RoundingMode.HALF_EVEN));
                    }
                }

                // Filter out DTOs with zero sales
                List<SalesCategoryDTO> filteredSalesCategoryDTOS = salesCategoryDTOS.stream()
                        .filter(data -> data.getTotalSales().compareTo(BigDecimal.ZERO) > 0)
                        .toList();

                // Implement pagination logic here
                int start = Math.min(page * size, filteredSalesCategoryDTOS.size());
                int end = Math.min((page + 1) * size, filteredSalesCategoryDTOS.size());

                Page<SalesCategoryDTO> pagedResult = new PageImpl<>(
                        filteredSalesCategoryDTOS.subList(start, end),
                        PageRequest.of(page, size),
                        filteredSalesCategoryDTOS.size()
                );

                return pagedResult;

            } catch (RuntimeException e) {
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        },asyncExecutor);
    }
}
