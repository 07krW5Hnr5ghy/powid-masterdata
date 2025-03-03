package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.Purchase;
import com.proyect.masterdata.domain.PurchaseType;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.repository.PurchaseRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Repository
public class PurchaseRepositoryCustomImpl implements PurchaseRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<Purchase> searchForPurchase(
            UUID clientId,
            String ref,
            String warehouse,
            String purchaseType,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Purchase> criteriaQuery = criteriaBuilder.createQuery(Purchase.class);

        Root<Purchase> itemRoot = criteriaQuery.from(Purchase.class);
        Join<Purchase, Warehouse> purchaseWarehouseJoin = itemRoot.join("warehouse");
        Join<Purchase, PurchaseType> purchasePurchaseTypeJoin = itemRoot.join("purchaseType");

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(
                clientId,
                ref,
                warehouse,
                purchaseType,
                status,
                criteriaBuilder,
                itemRoot,
                purchaseWarehouseJoin,
                purchasePurchaseTypeJoin);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> purchaseList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                purchaseList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                purchaseList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(purchaseList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<Purchase> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(
                clientId,
                ref,
                warehouse,
                purchaseType,
                status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicate(
            UUID clientId,
            String ref,
            String warehouse,
            String purchaseType,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<Purchase> itemRoot,
            Join<Purchase, Warehouse> purchaseWarehouseJoin,
            Join<Purchase, PurchaseType> purchasePurchaseTypeJoin) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if(ref != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(itemRoot.get("ref")),"%"+ref.toUpperCase()+"%"));
        }

        if(warehouse != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(purchaseWarehouseJoin.get("name")),"%"+warehouse.toUpperCase()+"%"));
        }

        if(purchaseType!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(purchaseWarehouseJoin.get("name")),"%"+purchaseType.toUpperCase()+"%"));
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<Purchase> itemRoot) {

        List<Order> purchaseList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            purchaseList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("warehouseId")) {
            purchaseList.add(criteriaBuilder.asc(itemRoot.get("warehouseId")));
        }

        if (sortColumn.equalsIgnoreCase("purchaseTypeId")) {
            purchaseList.add(criteriaBuilder.asc(itemRoot.get("purchaseTypeId")));
        }

        if (sortColumn.equalsIgnoreCase("purchaseSerial")) {
            purchaseList.add(criteriaBuilder.asc(itemRoot.get("purchaseSerial")));
        }

        return purchaseList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<Purchase> itemRoot) {

        List<Order> purchaseList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            purchaseList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("warehouseId")) {
            purchaseList.add(criteriaBuilder.desc(itemRoot.get("warehouseId")));
        }

        if (sortColumn.equalsIgnoreCase("purchaseTypeId")) {
            purchaseList.add(criteriaBuilder.desc(itemRoot.get("purchaseTypeId")));
        }

        if (sortColumn.equalsIgnoreCase("purchaseSerial")) {
            purchaseList.add(criteriaBuilder.desc(itemRoot.get("purchaseSerial")));
        }

        return purchaseList;
    }

    private Long getOrderCount(
            UUID clientId,
            String ref,
            String warehouse,
            String purchaseType,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<Purchase> itemRoot = criteriaQuery.from(Purchase.class);
        Join<Purchase, Warehouse> purchaseWarehouseJoin = itemRoot.join("warehouse");
        Join<Purchase, PurchaseType> purchasePurchaseTypeJoin = itemRoot.join("purchaseType");
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                ref,
                warehouse,
                purchaseType,
                status,
                criteriaBuilder,
                itemRoot,
                purchaseWarehouseJoin,
                purchasePurchaseTypeJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
