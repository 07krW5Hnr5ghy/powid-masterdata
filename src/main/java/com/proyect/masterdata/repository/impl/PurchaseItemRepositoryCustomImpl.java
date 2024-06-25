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
            List<Long> supplierIds,
            List<Long> supplierProductIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<PurchaseItem> criteriaQuery = criteriaBuilder.createQuery(PurchaseItem.class);

        Root<PurchaseItem> itemRoot = criteriaQuery.from(PurchaseItem.class);
        Join<PurchaseItem, Purchase> purchaseItemPurchaseJoin = itemRoot.join("purchase");

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(
                clientId,
                purchaseIds,
                supplierIds,
                supplierProductIds,
                status,
                criteriaBuilder,
                itemRoot,
                purchaseItemPurchaseJoin);

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

        TypedQuery<PurchaseItem> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(
                clientId,
                purchaseIds,
                supplierIds,
                supplierProductIds,
                status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicate(
            Long clientId,
            List<Long> purchaseIds,
            List<Long> supplierIds,
            List<Long> supplierProductIds,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<PurchaseItem> itemRoot,
            Join<PurchaseItem,Purchase> purchaseItemPurchaseJoin) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (!purchaseIds.isEmpty()) {
            conditions.add(criteriaBuilder.and(itemRoot.get("purchaseId").in(purchaseIds)));
        }

        if (!supplierIds.isEmpty()) {
            conditions.add(criteriaBuilder.and(purchaseItemPurchaseJoin.get("supplierId").in(supplierIds)));
        }

        if(!supplierProductIds.isEmpty()){
            conditions.add(criteriaBuilder.and(itemRoot.get("supplierProductId").in(supplierProductIds)));
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
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

        List<Order> purchaseItemList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            purchaseItemList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("purchaseId")) {
            purchaseItemList.add(criteriaBuilder.desc(itemRoot.get("purchaseId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            purchaseItemList.add(criteriaBuilder.desc(itemRoot.get("supplierProductId")));
        }

        return purchaseItemList;
    }

    private Long getOrderCount(
            Long clientId,
            List<Long> purchaseIds,
            List<Long> supplierIds,
            List<Long> supplierProductIds,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<PurchaseItem> itemRoot = criteriaQuery.from(PurchaseItem.class);
        Join<PurchaseItem,Purchase> purchaseItemPurchaseJoin = itemRoot.join("purchase");
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                purchaseIds,
                supplierIds,
                supplierProductIds,
                status,
                criteriaBuilder,
                itemRoot,
                purchaseItemPurchaseJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }

}
