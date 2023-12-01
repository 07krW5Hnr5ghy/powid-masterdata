package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.MembershipPayment;
import com.proyect.masterdata.repository.PaymentRepositoryCustom;
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

@Repository
public class PaymentRepositoryCustomImpl implements PaymentRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<MembershipPayment> searchForPayment(
            Double totalPayment,
            String month,
            Long idChannel,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MembershipPayment> criteriaQuery = criteriaBuilder.createQuery(MembershipPayment.class);
        Root<MembershipPayment> itemRoot = criteriaQuery.from(MembershipPayment.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(totalPayment, month, idChannel, criteriaBuilder, itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {
            List<Order> paymentList = new ArrayList<>();
            if (sort.equalsIgnoreCase("ASC")) {
                paymentList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }
            if (sort.equalsIgnoreCase("DESC")) {
                paymentList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }
            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(paymentList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<MembershipPayment> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        long count = getOrderCount(totalPayment, month, idChannel);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    public List<Predicate> predicateConditions(
            Double totalPayment,
            String month,
            Long idChannel,
            CriteriaBuilder criteriaBuilder,
            Root<MembershipPayment> itemRoot) {
        List<Predicate> conditions = new ArrayList<>();

        if (totalPayment != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    itemRoot.get("totalPayment"), totalPayment)));
        }

        if (month != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    criteriaBuilder.upper(itemRoot.get("month")), month.toUpperCase())));
        }

        if (idChannel != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    itemRoot.get("idChannel"), idChannel)));
        }

        return conditions;
    }

    List<Order> listASC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<MembershipPayment> itemRoot) {
        List<Order> paymentList = new ArrayList<>();
        if (sortColumn.equalsIgnoreCase("totalPayment")) {
            paymentList.add(criteriaBuilder.asc(itemRoot.get("totalPayment")));
        }
        if (sortColumn.equalsIgnoreCase("month")) {
            paymentList.add(criteriaBuilder.asc(itemRoot.get("month")));
        }
        return paymentList;
    }

    List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<MembershipPayment> itemRoot) {
        List<Order> paymentList = new ArrayList<>();
        if (sortColumn.equalsIgnoreCase("totalPayment")) {
            paymentList.add(criteriaBuilder.desc(itemRoot.get("totalPayment")));
        }
        if (sortColumn.equalsIgnoreCase("month")) {
            paymentList.add(criteriaBuilder.desc(itemRoot.get("month")));
        }
        return paymentList;
    }

    private long getOrderCount(Double totalPayment, String month, Long idChannel) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<MembershipPayment> itemRoot = criteriaQuery.from(MembershipPayment.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(totalPayment, month, idChannel, criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
