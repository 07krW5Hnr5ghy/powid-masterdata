package com.proyect.masterdata.repository.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.proyect.masterdata.domain.*;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.repository.SupplyOrderItemRepositoryCustom;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;

@Repository
public class SupplyOrderItemRepositoryCustomImpl implements SupplyOrderItemRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<SupplyOrderItem> searchForPurchaseItem(
            UUID clientId,
            Long purchaseNumber,
            String warehouse,
            String model,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<SupplyOrderItem> criteriaQuery = criteriaBuilder.createQuery(SupplyOrderItem.class);

<<<<<<< HEAD:src/main/java/com/proyect/masterdata/repository/impl/SupplyOrderItemRepositoryCustomImpl.java
        Root<SupplyOrderItem> itemRoot = criteriaQuery.from(SupplyOrderItem.class);
        Join<SupplyOrderItem, SupplyOrder> purchasePurchaseItemJoin = itemRoot.join("supplyOrder");
        Join<SupplyOrderItem, Product> purchaseItemProductJoin = purchasePurchaseItemJoin.join("product");
        Join<SupplyOrder, Warehouse> purchaseWarehouseJoin = purchasePurchaseItemJoin.join("warehouse");
=======
        Root<PurchaseItem> itemRoot = criteriaQuery.from(PurchaseItem.class);
        Join<PurchaseItem, Purchase> purchasePurchaseItemJoin = itemRoot.join("purchase");
        Join<PurchaseItem, Product> purchaseItemProductJoin = purchasePurchaseItemJoin.join("product");
        Join<Purchase, Warehouse> purchaseWarehouseJoin = purchasePurchaseItemJoin.join("warehouse");
>>>>>>> 0ceaf282c4cc63fc1280064498b8b7e9b3e0ca9a:src/main/java/com/proyect/masterdata/repository/impl/PurchaseItemRepositoryCustomImpl.java
        Join<Product, Model> productModelJoin = purchaseItemProductJoin.join("model");

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(
                clientId,
                purchaseNumber,
                warehouse,
                model,
                criteriaBuilder,
                itemRoot,
                purchasePurchaseItemJoin,
                productModelJoin,
                purchaseWarehouseJoin);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> purchaseItemList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                purchaseItemList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                purchaseItemList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(purchaseItemList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<SupplyOrderItem> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(
                clientId,
                purchaseNumber,
                warehouse,
                model);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicate(
            UUID clientId,
            Long purchaseNumber,
            String warehouse,
            String model,
            CriteriaBuilder criteriaBuilder,
<<<<<<< HEAD:src/main/java/com/proyect/masterdata/repository/impl/SupplyOrderItemRepositoryCustomImpl.java
            Root<SupplyOrderItem> itemRoot,
            Join<SupplyOrderItem, SupplyOrder> purchaseItemPurchaseJoin,
            Join<Product,Model> productModelJoin,
            Join<SupplyOrder,Warehouse> purchaseWarehouseJoin) {
=======
            Root<PurchaseItem> itemRoot,
            Join<PurchaseItem, Purchase> purchaseItemPurchaseJoin,
            Join<Product,Model> productModelJoin,
            Join<Purchase,Warehouse> purchaseWarehouseJoin) {
>>>>>>> 0ceaf282c4cc63fc1280064498b8b7e9b3e0ca9a:src/main/java/com/proyect/masterdata/repository/impl/PurchaseItemRepositoryCustomImpl.java

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if(purchaseNumber==null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(purchaseItemPurchaseJoin.get("purchaseNumber"), clientId)));
        }

        if (warehouse!=null) {
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(purchaseWarehouseJoin.get("name")),"%"+warehouse.toUpperCase()+"%"));
        }

        if (model != null) {
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productModelJoin.get("name")),"%"+model.toUpperCase()+"%"));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<SupplyOrderItem> itemRoot) {

        List<Order> purchaseItemList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            purchaseItemList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("purchaseId")) {
            purchaseItemList.add(criteriaBuilder.asc(itemRoot.get("purchaseId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            purchaseItemList.add(criteriaBuilder.asc(itemRoot.get("supplierProductId")));
        }

        return purchaseItemList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<SupplyOrderItem> itemRoot) {

        List<Order> purchaseList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            purchaseList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("purchaseId")) {
            purchaseList.add(criteriaBuilder.desc(itemRoot.get("purchaseId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            purchaseList.add(criteriaBuilder.desc(itemRoot.get("supplierProductId")));
        }

        return purchaseList;
    }

    private Long getOrderCount(
            UUID clientId,
            Long purchaseNumber,
            String warehouse,
            String model) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
<<<<<<< HEAD:src/main/java/com/proyect/masterdata/repository/impl/SupplyOrderItemRepositoryCustomImpl.java
        Root<SupplyOrderItem> itemRoot = criteriaQuery.from(SupplyOrderItem.class);
        Join<SupplyOrderItem, SupplyOrder> purchasePurchaseItemJoin = itemRoot.join("supplyOrder");
        Join<SupplyOrderItem, Product> purchaseItemProductJoin = purchasePurchaseItemJoin.join("product");
        Join<SupplyOrder, Warehouse> purchaseWarehouseJoin = purchasePurchaseItemJoin.join("warehouse");
=======
        Root<PurchaseItem> itemRoot = criteriaQuery.from(PurchaseItem.class);
        Join<PurchaseItem, Purchase> purchasePurchaseItemJoin = itemRoot.join("purchase");
        Join<PurchaseItem, Product> purchaseItemProductJoin = purchasePurchaseItemJoin.join("product");
        Join<Purchase, Warehouse> purchaseWarehouseJoin = purchasePurchaseItemJoin.join("warehouse");
>>>>>>> 0ceaf282c4cc63fc1280064498b8b7e9b3e0ca9a:src/main/java/com/proyect/masterdata/repository/impl/PurchaseItemRepositoryCustomImpl.java
        Join<Product, Model> productModelJoin = purchaseItemProductJoin.join("model");
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                purchaseNumber,
                warehouse,
                model,
                criteriaBuilder,
                itemRoot,
                purchasePurchaseItemJoin,
                productModelJoin,
                purchaseWarehouseJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }

}
