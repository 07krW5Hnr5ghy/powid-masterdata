package com.proyect.masterdata.repository.impl;

import java.util.ArrayList;
import java.util.List;

import com.proyect.masterdata.domain.Purchase;
import com.proyect.masterdata.domain.PurchaseItem;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.repository.PurchaseItemRepositoryCustom;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;

@Repository
public class PurchaseItemRepositoryCustomImpl implements PurchaseItemRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<PurchaseItem> searchForPurchaseItem(
            Long clientId,
            List<Long> purchaseIds,
            List<Long> warehouseIds,
            List<Long> supplierProductIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<PurchaseItem> criteriaQuery = criteriaBuilder.createQuery(PurchaseItem.class);

        Root<PurchaseItem> itemRoot = criteriaQuery.from(PurchaseItem.class);
        Join<PurchaseItem, Purchase> purchasePurchaseItemJoin = itemRoot.join("purchase");

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(
                clientId,
                purchaseIds,
                warehouseIds,
                supplierProductIds,
                criteriaBuilder,
                itemRoot,
                purchasePurchaseItemJoin);

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

        TypedQuery<PurchaseItem> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(
                clientId,
                purchaseIds,
                warehouseIds,
                supplierProductIds);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicate(
            Long clientId,
            List<Long> purchaseIds,
            List<Long> warehouseIds,
            List<Long> supplierProductIds,
            CriteriaBuilder criteriaBuilder,
            Root<PurchaseItem> itemRoot,
            Join<PurchaseItem, Purchase> purchaseItemPurchaseJoin) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (!purchaseIds.isEmpty()) {
            conditions.add(criteriaBuilder.and(itemRoot.get("purchaseId").in(purchaseIds)));
        }

        if (!warehouseIds.isEmpty()) {
            conditions.add(criteriaBuilder.and(purchaseItemPurchaseJoin.get("warehouseId").in(warehouseIds)));
        }

        if(!supplierProductIds.isEmpty()){
            conditions.add(criteriaBuilder.and(itemRoot.get("supplierProductId").in(supplierProductIds)));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<PurchaseItem> itemRoot) {

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

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<PurchaseItem> itemRoot) {

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
            Long clientId,
            List<Long> purchaseIds,
            List<Long> warehouseIds,
            List<Long> supplierProductIds) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<PurchaseItem> itemRoot = criteriaQuery.from(PurchaseItem.class);
        Join<PurchaseItem, Purchase> purchasePurchaseItemJoin = itemRoot.join("purchase");
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                purchaseIds,
                warehouseIds,
                supplierProductIds,
                criteriaBuilder,
                itemRoot,
                purchasePurchaseItemJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }

}
