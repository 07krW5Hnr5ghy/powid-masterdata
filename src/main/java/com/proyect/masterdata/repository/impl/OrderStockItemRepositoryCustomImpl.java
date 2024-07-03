package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.OrderStock;
import com.proyect.masterdata.domain.OrderStockItem;
import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.repository.OrderStockItemRepositoryCustom;
import io.micrometer.common.util.StringUtils;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;

@Repository
public class OrderStockItemRepositoryCustomImpl implements OrderStockItemRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<OrderStockItem> searchForOrderStockItem(
            Long clientId,
            List<Long> orderIds,
            List<Long> warehouseIds,
            List<Long> productIds,
            List<Long> supplierProductIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<OrderStockItem> criteriaQuery = criteriaBuilder.createQuery(OrderStockItem.class);
        Root<OrderStockItem> itemRoot = criteriaQuery.from(OrderStockItem.class);
        Join<OrderStockItem, OrderStock> orderStockItemOrderStockJoin = itemRoot.join("orderStock");
        Join<OrderStockItem, SupplierProduct> orderStockItemSupplierProductJoin = itemRoot.join("supplierProduct");

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(
                clientId,
                orderIds,
                warehouseIds,
                productIds,
                supplierProductIds,
                status,
                criteriaBuilder,
                itemRoot,
                orderStockItemOrderStockJoin,
                orderStockItemSupplierProductJoin);
        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> orderStockList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                orderStockList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                orderStockList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(orderStockList);

        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<OrderStockItem> orderingTypedQuery = entityManager.createQuery(criteriaQuery);
        orderingTypedQuery.setFirstResult(pageNumber * pageSize);
        orderingTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber,pageSize);
        Long count = getOrderCount(
                clientId,
                orderIds,
                warehouseIds,
                productIds,
                supplierProductIds,
                status);
        return new PageImpl<>(orderingTypedQuery.getResultList(),pageable,count);
    }

    List<Predicate> predicateConditions(
            Long clientId,
            List<Long> orderIds,
            List<Long> warehouseIds,
            List<Long> productIds,
            List<Long> supplierProductIds,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<OrderStockItem> itemRoot,
            Join<OrderStockItem, OrderStock> orderStockItemOrderStockJoin,
            Join<OrderStockItem, SupplierProduct> orderStockItemSupplierProductJoin){
        List<Predicate> conditions = new ArrayList<>();

        if(clientId != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"),clientId)));
        }

        if(!orderIds.isEmpty()){
            conditions.add(criteriaBuilder.and(orderStockItemOrderStockJoin.get("orderId").in(orderIds)));
        }

        if(!warehouseIds.isEmpty()){
            conditions.add(criteriaBuilder.and(orderStockItemOrderStockJoin.get("warehouseId").in(warehouseIds)));
        }

        if(!productIds.isEmpty()){
            conditions.add(criteriaBuilder.and(orderStockItemSupplierProductJoin.get("productId").in(productIds)));
        }

        if(!supplierProductIds.isEmpty()){
            conditions.add(criteriaBuilder.and(itemRoot.get("supplierProductId").in(supplierProductIds)));
        }

        if (status){
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status){
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn,CriteriaBuilder criteriaBuilder,Root<OrderStockItem> itemRoot){
        List<Order> orderStockList = new ArrayList<>();

        if(sortColumn.equals("supplierProductId")){
            orderStockList.add(criteriaBuilder.asc(itemRoot.get("supplierProductId")));
        }

        if(sortColumn.equals("orderId")){
            orderStockList.add(criteriaBuilder.asc(itemRoot.get("orderId")));
        }

        if(sortColumn.equals("clientId")){
            orderStockList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        return orderStockList;
    }

    private List<Order> listDESC(String sortColumn,CriteriaBuilder criteriaBuilder,Root<OrderStockItem> itemRoot){
        List<Order> orderStockList = new ArrayList<>();

        if(sortColumn.equals("supplierProductId")){
            orderStockList.add(criteriaBuilder.desc(itemRoot.get("supplierProductId")));
        }

        if(sortColumn.equals("orderId")){
            orderStockList.add(criteriaBuilder.desc(itemRoot.get("orderId")));
        }

        if(sortColumn.equals("clientId")){
            orderStockList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        return orderStockList;
    }

    private Long getOrderCount(
            Long clientId,
            List<Long> orderIds,
            List<Long> warehouseIds,
            List<Long> productIds,
            List<Long> supplierProductIds,
            Boolean status){
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<OrderStockItem> itemRoot = criteriaQuery.from(OrderStockItem.class);
        Join<OrderStockItem, OrderStock> orderStockItemOrderStockJoin = itemRoot.join("orderStock");
        Join<OrderStockItem, SupplierProduct> orderStockItemSupplierProductJoin = itemRoot.join("supplierProduct");

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(
                clientId,
                orderIds,
                warehouseIds,
                productIds,
                supplierProductIds,
                status,
                criteriaBuilder,
                itemRoot,
                orderStockItemOrderStockJoin,
                orderStockItemSupplierProductJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
