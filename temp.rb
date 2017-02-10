# rubocop:disable Style/FrozenStringLiteralComment

# rubocop:disable Rails/Render
class PullRequestsController < AbstractRepositoryController
  areas_of_responsibility :code_collab, :pull_requests

  include ShowPartial, PrefetchHelper, ProgressiveTimeline, DiscussionPreloadHelper,
    ControllerMethods::Diffs

  helper :compare
  around_filter :select_up_to_date_database, only: :show_partial
  before_filter :login_required, only: [:create, :comment, :merge_button, :sync, :dismiss_protip]
  before_filter :login_required_redirect_for_public_repo, only: [:new]
  before_filter :non_migrating_repository_required,
    except: [:new, :show, :show_partial, :merge_button, :diff, :patch, :merge_button_matrix]
  before_filter :check_for_empty_repository, only: [:create, :new]
  before_filter :ensure_pull_head_pushable, only: [:cleanup, :undo_cleanup, :sync]
  before_filter :content_authorization_required, only: [:create, :merge]
  skip_before_filter :cap_pagination, unless: :robot?

  layout :repository_layout

  param_encoding :create, :base, "ASCII-8BIT"
  param_encoding :create, :head, "ASCII-8BIT"

  def new
    redirect_to compare_path(current_repository, params[:range], true)
  end

  def create
    repo = current_repository
    return render_404 unless repo
    return if reject_bully_for?(repo)

    params[:pull_request] ||= {}
    options = params.slice(:base, :head)
    options[:user] = current_user

    params[:issue] ||= {}
    params[:issue][:title] = params[:pull_request][:title]
    params[:issue][:body]  = params[:pull_request][:body]
    options[:issue] = build_issue
    options[:collab_privs] = !!params[:collab_privs]
    options[:reviewer_ids] = params[:reviewer_ids]

    begin
      @pull_request = PullRequest.create_for!(repo, options)
      @comparison   = @pull_request.comparison
      @issue        = @pull_request.issue

      GitHub.instrument "pull_request.create", user: current_user
      instrument_issue_creation_via_ui(@pull_request)
      instrument_saved_reply_use(params[:saved_reply_id], "pull_request")
      # Keep track of if a pull request targets the repo's
      # default branch, or one in progress.
      if options[:base].strip == "#{repo.owner}:#{repo.default_branch}"
        GitHub.stats.increment("pullrequest.target.default")
      else
        GitHub.stats.increment("pullrequest.target.other")
      end

      if params[:quick_pull].present?
        type = @pull_request.cross_repo? ? "cross" : "same"
        GitHub.stats.increment("pullrequest.quick.total.create")
        GitHub.stats.increment("pullrequest.quick.#{type}.create")
      end

      redirect_to pull_request_path(@pull_request, repo)
    rescue ActiveRecord::RecordInvalid => e
      flash[:error] = "Pull request creation failed. #{human_failure_message(e)}"
      range = [options[:base], options[:head]].compact.join("...")
      redirect_to compare_path(repo, range, true)
    end
  end

  def show
    @mobile_view_available = true

    redirect_or_error = ensure_valid_pull_request

    if performed?
      return redirect_or_error
    end

    if params[:range]
      env["pull_request.timeout_reason"] = "compute_diff"

      if oids = parse_show_range_oid_components(@pull, params[:range])
        oid1, oid2 = oids
      else
        return render template: "pull_requests/bad_range", status: :not_found
      end

      allowed_commits = @pull.changed_commit_oids
      if !(oid1.nil? || allowed_commits.include?(oid1)) || !allowed_commits.include?(oid2)
        return render template: "pull_requests/bad_range", status: :not_found
      end

      if oid1 && (oid1 == oid2 || !@pull.repository.rpc.descendant_of([[oid2, oid1]]).values.first)
        return render template: "pull_requests/bad_range", status: :not_found
      end

      expected_canonical_range = oid1 ? "#{oid1}..#{oid2}" : "#{oid2}"
      if params[:range] != expected_canonical_range
        return redirect_to(range: expected_canonical_range)
      end

      if params[:tab] == "commits"
        @specified_tab = "files"
        oid1 = current_repository.commits.find(oid2).parent_oids.first
      end

      @comparison = @pull.historical_comparison
      merge_base_oid = @comparison.compare_repository.rpc.best_merge_base(@pull.base_sha, oid2)

      if merge_base_oid.nil?
        return render template: "pull_requests/orphan_commit", status: :not_found, locals: { oid2: oid2 }
      end

      oid1 ||= @pull.compare_repository.rpc.best_merge_base(oid2, merge_base_oid) if merge_base_oid
      unless @pull_comparison = PullRequest::Comparison.find(pull: @pull, start_commit_oid: oid1, end_commit_oid: oid2, base_commit_oid: merge_base_oid)
        return render template: "pull_requests/bad_range", status: :not_found
      end

      load_diff

      env["pull_request.timeout_reason"] = nil
    else
      @comparison = @pull.comparison

      if specified_tab == "files"
        env["pull_request.timeout_reason"] = "compute_diff"

        start_oid, end_oid = @pull.merge_base, @pull.head_sha
        if start_oid && end_oid
          begin
            start_commit, end_commit = @pull.compare_repository.commits.find([start_oid, end_oid])
            @pull_comparison = PullRequest::Comparison.new(pull: @pull, start_commit: start_commit, end_commit: end_commit, base_commit: start_commit)

            load_diff
          rescue GitRPC::ObjectMissing
          end
        end

        env["pull_request.timeout_reason"] = nil
      end
    end

    prefetch_deferred do
      if specified_tab == "files" && @pull_comparison && logged_in?
        @pull_comparison.mark_as_seen(user: current_user)
        mark_thread_as_read @pull.issue
      elsif specified_tab == "discussion"
        mark_thread_as_read @pull.issue
      end
    end

    if prefetch_viewed?
      return head(:no_content)
    end

    @labels = current_repository.sorted_labels

    prepare_for_rendering(timeline: specified_tab == "discussion", pull_comparison: @pull_comparison)

    unless "discussion" == specified_tab
      override_analytics_location "/<user-name>/<repo-name>/pull_requests/show/#{specified_tab}"
    end

    if show_mobile_view? && (request_category != "raw")
      return render_template_view("mobile/pull_requests/show", Mobile::PullRequests::ShowPageView, {
        pull: @pull,
        diffs: @pull_comparison.try(:diffs),
        tab: specified_tab,
        visible_timeline_items: visible_timeline_items,
        showing_full_timeline: showing_full_timeline?
      }, layout: "mobile/application")
    end

    respond_to do |format|
      format.html do
        if params[:shdds]
          @syntax_highlighted_diffs_forced = true
          render_to_string
          head :no_content
        else
          if logged_in?
            @current_review = @pull.latest_pending_review_for(current_user)
          end

          if specified_tab == "files"
            if @pull_comparison.nil?
              render "pull_requests/files_unavailable"
            else
              render "pull_requests/files"
            end
          elsif specified_tab == "commits"
            render "pull_requests/commits"
          else
            render "pull_requests/conversation"
          end
        end
      end
    end
  end

  # Build enough of a pull request object to render the requested reviewer
  # sidebar template on the pull request create page.
  #
  # Params: reviewer_ids - An array of User IDs.
  #
  # Returns an unsaved PullRequest.
  def build_pull
    pull =
      if params[:range].present? && suggested_reviewers_enabled?
        comparison = GitHub::Comparison.from_range(current_repository, params[:range])
        if comparison.valid? && comparison.viewable_by?(current_user)
          comparison.build_pull_request(user: current_user)
        end
      else
        current_repository.pull_requests.build(user: current_user)
      end

    pull.issue = current_repository.issues.build

    if current_repository.pushable_by?(current_user)
      if reviewer_ids = params[:reviewer_ids]
        reviewer_ids.select(&:present?).each do |id|
          pull.review_requests.build(reviewer_id: id)
        end
      end
    end

    pull
  end

  TIMELINE_PARTIALS = %w[
    pull_requests/timeline
    pull_requests/timeline_marker
  ].freeze

  VALID_REASONS = %w[
    view-more-button
    expose-fragment
  ].freeze

  def show_partial
    partial = params[:partial]
    return head :not_found unless valid_partial?(partial)

    GitHub.stats.time("pullrequest.show_partial_find") do
      @pull =
        if params[:id]
          PullRequest.find_by_number_and_repo(params[:id].to_i, current_repository)
        else
          build_pull
        end
    end

    return head :not_found unless @pull

    if partial == "pull_requests/timeline"
      if focused_timeline? && visible_timeline_items.empty?
        return head :bad_request
      end

      reason = VALID_REASONS.include?(params[:reason]) ? params[:reason] : "other"
      GitHub.stats.increment("pullrequest.show_partial.timeline.#{reason}.count")
    end

    if partial == "issues/sidebar/new/reviewers"
      if suggested_reviewers_enabled?
        suggested_reviewers = @pull.suggested_reviewers
      end
    else
      prepare_for_rendering(timeline: TIMELINE_PARTIALS.include?(partial), pull_comparison: nil)
    end

    GitHub.stats.time("pullrequest.show_partial_render") do
      respond_to do |format|
        format.html do
          render partial: partial, object: @pull, layout: false, locals: {
            pull: @pull,
            merge_type: params[:merge_type],
            suggested_reviewers: suggested_reviewers
          }
        end
      end
    end
  end

  def short_method
    "this just returns this string"
  end

  def show_partial_commit
    partial = params[:partial]
    return head :not_found unless valid_partial?(partial)

    unless pull = PullRequest.find_by_number_and_repo(params[:id].to_i, current_repository)
      return head :not_found
    end

    unless commit = current_commit
      return head :not_found
    end

    Commit.prefill_combined_statuses([current_commit], current_repository)

    GitHub.stats.time("pullrequest.show_partial_render") do
      respond_to do |format|
        format.html do
          render partial: partial, object: commit, layout: false, locals: { pull: pull, commit: commit }
        end
      end
    end
  end

  def show_partial_comparison
    partial = params[:partial]
    return head :not_found unless valid_partial?(partial)

    unless pull = PullRequest.find_by_number_and_repo(params[:id].to_i, current_repository)
      return head :not_found
    end

    unless pull_comparison = PullRequest::Comparison.find(pull: pull, start_commit_oid: params[:start_commit_oid], end_commit_oid: params[:end_commit_oid], base_commit_oid: params[:base_commit_oid])
      return head :not_found
    end

    GitHub.stats.time("pullrequest.show_partial_render") do
      respond_to do |format|
        format.html do
          render partial: partial, locals: { pull: pull, pull_comparison: pull_comparison }
        end
      end
    end
  end

  def show_toc
    pull = PullRequest.find_by_number_and_repo(params[:id], current_repository)
    return head :not_found unless pull

    return head :not_found unless valid_sha_param?(:sha1) &&
                                  valid_sha_param?(:sha2) && params[:sha2].present? &&
                                  valid_sha_param?(:base_sha)

    diff_options = { base_sha: params[:base_sha] }
    diff = GitHub::Diff.new(pull.compare_repository, params[:sha1], params[:sha2], diff_options)

    respond_to do |format|
      format.html do
        render partial: "pull_requests/diffbar/toc_menu_items",
          layout: false,
          locals: {
            summary_delta_views: diff.summary.deltas.map { |d| Diff::SummaryDeltaView.new(d) },
            diff: diff
          }
      end
    end
  end

  def cleanup
    stats_key  = ["pullrequest",
                  "delete_button",
                  @pull.cross_repo? ? "cross_repo" : "same_repo"].join(".")

    # The branch was deleted by someone else before this user
    # clicked the button. Send us to the bottom.
    if !@pull.head_ref_exist?
      raise Git::Ref::NotFound
    end

    result = @pull.cleanup_head_ref(current_user,
                              request_reflog_data("pull request branch delete button"))

    GitHub.stats.increment("#{stats_key}.#{result ? "success" : "error"}")

    if request.xhr?
      render_head_ref_update
    else
      if result
        flash[:notice] = "Branch deleted successfully."
      else
        flash[:error] = "Oops, something went wrong."
      end

      redirect_to pull_request_path(@pull)
    end

  # We can get here both from above where the branch has already
  # been deleted before the button is clicked, or a race condition
  # where the application code thinks the branch exists but by
  # the time we execute the git command, someone else has already deleted it.
  rescue Git::Ref::NotFound
    GitHub.stats.increment("#{stats_key}.already_deleted")
    render_head_ref_update
  end

  def undo_cleanup
    stats_key  = ["pullrequest",
                  "delete_button_undo",
                  @pull.cross_repo? ? "cross_repo" : "same_repo"].join(".")

    if @pull.restore_head_ref(current_user,
                              request_reflog_data("pull request branch undo button"))
      GitHub.stats.increment("#{stats_key}.success")
    else
      GitHub.stats.increment("#{stats_key}.error")
    end

    render_head_ref_update
  end

  def merge
    @pull = current_repository.issues.find_by_number(params[:id].to_i).try(:pull_request)
    return render_404 unless @pull

    if params[:squash_commits] == "1"
      merge_method = "squash"
    elsif params[:do].present?
      merge_method = params[:do]
    else
      if current_repository.merge_commit_allowed?
        merge_method = "merge"
      else
        merge_method = "squash"
      end
    end

    merge_method_allowed = case merge_method
      when "merge"
        current_repository.merge_commit_allowed?
      when "squash"
        current_repository.squash_merge_allowed?
      when "rebase"
        current_repository.rebase_merge_allowed?
      else
        false
    end

    if !merge_method_allowed
      result, message = nil, "The selected merge method (#{merge_method}) is not allowed."
    elsif @pull.git_merges_cleanly? and @pull.base_repository.pushable_by?(current_user)
      GitHub.stats.increment("pullrequest.merge_button.click")
      begin
        result, message = @pull.merge(current_user,
                                      message_title: params[:commit_title],
                                      message: params[:commit_message],
                                      reflog_data: request_reflog_data("pull request merge button"),
                                      expected_head: params[:head_sha],
                                      method: merge_method.to_sym)
      rescue Git::Ref::HookFailed => e
        @hook_out = e.message
        result, message = nil, "Pre-receive hooks failed. See below for details."
      end

      if merge_method == "squash"
        @pull.base_repository.set_sticky_merge_method(current_user, "squash")
      elsif merge_method == "merge"
        @pull.base_repository.set_sticky_merge_method(current_user, "merge_commit")
      end
    else
      result, message = nil, "We couldn’t merge this pull request. Reload the page before trying again."
    end

    if request.xhr?
      if result
        GitHub.dogstats.histogram("pull_request.merged.requested_reviewers.count", @pull.requested_reviewers.size)
        respond_to do |format|
          format.json do
            prepare_for_rendering(timeline: true, pull_comparison: nil)
            render_immediate_partials @pull, :timeline_marker, :sidebar, :merging, :form_actions
          end
        end
      else
        GitHub.stats.increment("pullrequest.merge_button.error")
        respond_to do |format|
          format.html do
            render status: :unprocessable_entity, partial: "pull_requests/merging_error", locals: {
              title: "Merge attempt failed",
              message: (message || "We couldn’t merge this pull request."),
              hook_output: @hook_out
            }
          end
        end
        # render :plain => message, :status => :unprocessable_entity
      end
    else
      if result
        GitHub.dogstats.histogram("pull_request.merged.requested_reviewers.count", @pull.requested_reviewers.size)
        redirect_to pull_request_path(@pull) + "#merged-event"
      else
        flash[:error] = message
        GitHub.stats.increment("pullrequest.merge_button.error")
        redirect_to pull_request_path(@pull)
      end
    end
  end

  def change_base
    @pull = find_pull_request
    return render_404 if !@pull || @pull.merged? ||
      !@pull.issue.can_modify?(current_user) ||
      !params[:new_base].present?

    new_base = URI.decode(params[:new_base])
    begin
      @pull.change_base_branch(current_user, new_base)
      flash[:notice] = "Updated base branch to #{new_base}."
    rescue PullRequest::BadComparison, PullRequest::AlreadyExists => e
      flash[:error] = e.ui_message
    end
    redirect_to pull_request_path(@pull)
  end

  def update_branch
    @pull = find_pull_request
    return render_404 unless @pull

    update_method = params[:update_method] == "rebase" ? :rebase : :merge

    begin
      @pull.merge_base_into_head(
        user: current_user,
        method: update_method,
        expected_head_oid: params[:expected_head_oid]
      )

      current_repository.set_sticky_update_method(current_user, update_method)

      if request.xhr?
        respond_to do |format|
          format.json do
            prepare_for_rendering(timeline: true, pull_comparison: nil)
            render_immediate_partials(@pull, :timeline_marker, :merging)
          end
        end
      else
        redirect_to pull_request_path(@pull) + "#partial-pull-merging"
      end
    rescue GitHub::UIError => e
      if request.xhr?
        respond_to do |format|
          format.html do
            render status: :unprocessable_entity, partial: "pull_requests/merging_error", locals: {
              title: "Update branch attempt failed",
              message: e.ui_message
            }
          end
        end
      else
        flash[:error] = e.ui_message
        redirect_to pull_request_path(@pull) + "#partial-pull-merging"
      end
    end
  end

  def revert
    @pull = find_pull_request
    return render_404 unless @pull && @pull.revertable_by?(current_user)

    stats_key = ["pullrequest",
                 "revert_button",
                 @pull.cross_repo? ? "cross_repo" : "same_repo"].join(".")

    begin
      revert_branch, error = @pull.revert(current_user, request_reflog_data("pull request revert button"))
      if revert_branch
        GitHub.stats.increment("#{stats_key}.success")

        base_label, head_label =
          if revert_branch.repository == @pull.base_repository
            [@pull.base_ref_name, revert_branch.name]
          else
            ["#{@pull.base_label(username_qualified: true)}", "#{revert_branch.repository.owner.login}:#{revert_branch.name}"]
          end

        flash[:pull_request] = {
          title: revert_branch.target.message,
          body: "Reverts #{@pull.base_repository.name_with_owner}##{@pull.number}"
        }
        redirect_to(compare_path(@pull.base_repository, "#{base_label}...#{head_label}", true))
      else
        if error == :merge_conflict
          GitHub.stats.increment("#{stats_key}.merge_conflict")
        else
          GitHub.stats.increment("#{stats_key}.error")
        end

        flash[:error] = "Sorry, this pull request couldn’t be reverted automatically. It may have \
                         already been reverted, or the content may have changed since it was merged."
        redirect_to pull_request_path(@pull)
      end
    rescue Git::Ref::HookFailed => e
      flash[:hook_out] = e.message
      flash[:hook_message] = "Pull request could not be reverted."
      redirect_to pull_request_path(@pull)
    end
  end

  def merge_button
    pull = current_repository.issues.find_by_number(params[:id].to_i).try(:pull_request)
    return render_404 if pull.nil?
    merge_state = pull.cached_merge_state(viewer: current_user)

    respond_to do |format|
      format.html do
        if merge_state.unknown?
          head :accepted
        elsif alt_merge_box_ui_enabled?
          render partial: "pull_requests/alt_ui/merge_button", locals: { pull: pull }
        else
          render partial: "pull_requests/merge_button", locals: { pull: pull }
        end
      end

      format.json do
        render json: {mergeable_state: merge_state.status}.to_json
      end
    end
  end

  def diff
    # This might be a request for a redirect to the PR for a branch name ending in .diff,
    # or might be a request for a numbered PR in .diff format.
    diff_ref = "#{params[:id]}.diff"
    if current_repository.heads.include?(diff_ref)
      return redirect_or_404(diff_ref)
    end

    redirect_or_error = ensure_valid_pull_request
    if performed?
      return redirect_or_error
    else
      redirect_to build_pull_request_diff_url
    end
  end

  def patch
    # This might be a request for a redirect to the PR for a branch name ending in .patch,
    # or might be a request for a numbered PR in .patch format.
    patch_ref = "#{params[:id]}.patch"
    if current_repository.heads.include?(patch_ref)
      return redirect_or_404(patch_ref)
    end

    redirect_or_error = ensure_valid_pull_request
    if performed?
      return redirect_or_error
    else
      redirect_to build_pull_request_patch_url
    end
  end

  def comment
    return if reject_bully?

    @pull = current_repository.issues.find_by_number(params[:id].to_i).try(:pull_request)
    return render_404 unless @pull
    issue = @pull.issue

    valid = true
    comment_body = params[:comment][:body]

    if !comment_body.nil? && !can_skip_creating_comment?
      comment = issue.comments.build(body: comment_body)
      comment.user       = current_user
      comment.repository = current_repository
      valid &= comment.save

    elsif params[:comment_and_close] == "1"
      comment = issue.comment_and_close(current_user, comment_body)
      valid &= comment if comment_body.present?

      GitHub.dogstats.histogram("pull_request.closed.requested_reviewers.count", @pull.requested_reviewers.size)

    elsif params[:comment_and_open] == "1"
      comment = issue.comment_and_open(current_user, comment_body)
      valid &= comment if comment_body.present?
    end

    mark_thread_as_read issue
    if valid && comment_body.present?
      GitHub.instrument "comment.create", user: current_user
      instrument_saved_reply_use(params[:saved_reply_id], "pull_request_comment")
    end

    respond_to do |format|
      format.json do
        if valid
          prepare_for_rendering(timeline: true, pull_comparison: nil)
          render_immediate_partials @pull, :timeline_marker, :sidebar, :merging, :form_actions, :title
        else
          errors = comment.errors.map { |attr, msg| msg }
          render json: { errors: errors }, status: :unprocessable_entity
        end
      end
      format.html do
        if valid
          anchor = comment ? "#issuecomment-#{comment.id}" : ""
          redirect_to pull_request_path(@pull) + anchor
        else
          flash[:error] = comment.errors.full_messages.to_sentence
          redirect_to :back
        end
      end
    end
  end

  def dismiss_protip
    current_user.dismiss_notice("continuous_integration_tip")

    head :ok
  end

  if Rails.env.development?
    def merge_button_matrix
      if mobile?
        return render(template: "pull_requests/merge_button_matrix_mobile.html", layout: "mobile/application")
      end
    end
  end

  def set_collab
    pull = current_repository.issues.find_by_number(params[:id].to_i).try(:pull_request)

    return render_404 if !pull || !pull.head_repository.repository.pushable_by?(current_user)

    if !!params[:collab_privs]
      pull.fork_collab = :allowed
    else
      pull.fork_collab = :denied
    end

    pull.save!

    redirect_to pull_request_path(pull)
  end

  def resolve_conflicts
    redirect_or_error = ensure_valid_pull_request

    unless logged_in? && @pull.head_repository && @pull.head_repository.pushable_by?(current_user, ref: @pull.head_ref_name)
      return render_404
    end
    return redirect_to pull_request_path(@pull) unless @pull.conflict_resolvable?

    if performed?
      return redirect_or_error
    end

    render "pull_requests/resolve_conflicts", locals: { pull: @pull }
  end

  def valid?
  end

protected

  helper_method :tab_specified?
  def tab_specified?(tab_name)
    specified_tab.to_s == tab_name.to_s
  end

  def id?
  end

  helper_method :pull_request_subscribe_enabled?
  def pull_request_subscribe_enabled?
    Rails.development? || Rails.test? || preview_features?
  end

  def specified_tab
    @specified_tab || params[:tab].presence || "discussion"
  end
  helper_method :specified_tab

  def valid_tab?
    params[:tab].blank? || %w{commits files tasks}.include?(params[:tab])
  end

  def val
  end

  def reject_bully_for?(repo)
    if blocked_by_owner?(repo.owner_id)
      flash[:error] = "You can't perform that action at this time."
      redirect_to repo.permalink
      true
    end
  end

  def reject_bully?
    reject_bully_for? current_repository
  end

  def working_predicate?
  end

  def tree_name
    if @pull && @pull.open?
      @pull.head_ref_name
    elsif @pull
      @pull.head_sha
    else
      super
    end
  end

  private

  def find_pull_request
    PullRequest.find_by_number_and_repo(params[:id].to_i, current_repository,
      include: [{issue: { comments: :user }}])
  end

  # Private: For a given ref name, redirect to the appropriate pull request
  # path if one exists, or 404 otherwise.
  #
  # Returns the redirect or render_404 result.
  def redirect_or_404(ref)
    # redirect to number version if ref is a branch name,
    # redirect to new if ref is a branch with no pull request
    # 404 otherwise
    if pull = current_repository.pull_requests.for_branch(ref).last
      redirect_to pull_request_path(pull)
    elsif ref =~ /[:.]/ || current_repository.heads.include?(ref)
      redirect_to new_pull_request_path(range: ref)
    else
      render_404
    end
  end

  # Private: Validate the requested pull request ID. If necessary redirect to
  # a more appropriate URL or return a 404 if the PR isn't/shouldn't be
  # available.
  def ensure_valid_pull_request
    if params[:id] =~ /\D/
      return redirect_or_404(params[:id])
    end

    @pull = find_pull_request

    return redirect_to(issue_path(id: params[:id])) unless @pull
    return redirect_to(pull_request_path @pull) unless valid_tab?

    return render_404 if @pull.hide_from_user?(current_user)

    @prose_url_hints = { tab: "files" }

    @pull.set_diff_options(
      use_summary: true,
      ignore_whitespace: ignore_whitespace?
    )
  end

  # Validates the provided parameter is a valid sha, or nil
  def valid_sha_param?(param_name)
    param = params[param_name]
    param.nil? || param =~ /[0-9a-f]{40}/
  end

  def load_diff
    @pull_comparison.ignore_whitespace = ignore_whitespace?
    @pull_comparison.diff_options[:use_summary] = true

    # load diff data
    if !show_mobile_view?
      @pull_comparison.diff_options[:top_only] = true

      GitHub.dogstats.time("diff.load.initial", tags: dogstats_request_tags) do
        @pull_comparison.diffs.apply_auto_load_single_entry_limits!
        @pull_comparison.diffs.load_diff(timeout: request_time_left / 2)
      end
    else
      @pull_comparison.diffs.load_diff(timeout: request_time_left / 2)
    end
  end

  # Preload data needed for rendering the PR.
  #
  # timeline - Boolean specifying whether the PR's timeline will be rendered.
  # pull_comparison - PullRequest::Comparison specifying whether the PR's diff will be rendered.
  #
  # Returns nothing.
  def prepare_for_rendering(timeline:, pull_comparison:)
    prepare_for_rendering_timeline_items(timeline: timeline, pull_comparison: pull_comparison)
    prepare_for_rendering_diffs(timeline: timeline, pull_comparison: pull_comparison)
  end

  # Preload data needed for rendering timeline items.
  #
  # timeline - Boolean specifying whether the PR's timeline will be rendered.
  # pull_comparison - PullRequest::Comparison specifying whether the PR's diff
  #   (which contains timeline items corresponding to live review threads) will be rendered.
  #
  # Returns nothing.
  def prepare_for_rendering_timeline_items(timeline:, pull_comparison:)
    timeline_items = visible_timeline_items
    if pull_comparison
      # Add in the review threads that are shown on the Files changed tab.
      # Note that some of these might be also be shown on the Discussion tab,
      # but the AR objects will actually be distinct so it's still worth
      # prefilling/warming both objects.
      timeline_items = timeline_items.dup
      threads = pull_comparison.review_threads(viewer: current_user)
      timeline_items.concat(threads.to_a)
    end

    @pull.prefill_timeline_associations(timeline_items, preload_diff_entries: timeline)
    @pull.warm_timeline_caches(timeline_items)
    preload_discussion_group_data(timeline_items)
  end

  # Preload data needed for rendering diffs.
  #
  # timeline - Boolean specifying whether the PR's timeline (which contains
  #            diffs for review threads) will be rendered.
  # pull_comparison - PullRequest::Comparison specifying whether the PR's diff will be rendered.
  #
  # Returns nothing.
  def prepare_for_rendering_diffs(timeline:, pull_comparison:)
    return unless syntax_highlighted_diffs_enabled?

    diffs_to_highlight = []
    if timeline
      visible_timeline_items.each do |item|
        next unless item.is_a?(PullRequestReviewThread)
        next unless item.diff_entry
        diffs_to_highlight << item.diff_entry
      end
    end
    if pull_comparison
      diffs_to_highlight.concat(pull_comparison.diffs.to_a)
    end
    SyntaxHighlightedDiff.new(@pull.comparison.compare_repository, current_user).highlight!(diffs_to_highlight)
  end

  def can_skip_creating_comment?
    params[:comment_and_close].present? ||
      params[:comment_and_open].present?
  end

  # If it's empty, you can't issue a pull request.  There will be no
  # base SHA to merge against.
  def check_for_empty_repository
    if current_repository.empty?
      redirect_to current_repository
    end
  end

  def ensure_pull_head_pushable
    @pull = current_repository.issues.find_by_number(params[:id].to_i).try(:pull_request)

    if @pull.nil? || !@pull.head_repository.pushable_by?(current_user)
      render_404
    end
  end

  # Reload the pull so the deletable/restorable status is current,
  # then render updates for the event list and the merge/delete buttons.
  def render_head_ref_update
    @pull.reload
    respond_to do |format|
      format.json do
        prepare_for_rendering(timeline: true, pull_comparison: nil)
        render_immediate_partials(@pull, :timeline_marker, :merging, :form_actions)
      end
    end
  end

  # Internal: Provide a better failure message than simply taking the validation errors
  #
  # e - the ActiveRecord::RecordInvalid exception from the creation failure
  #
  # Returns a String
  def human_failure_message(e)
    pr = e.record

    # e.record can be an Issue, raised in PullRequest.create_for.
    return e.message unless pr.is_a?(PullRequest)

    if bad_branches = pr.missing_refs
      if bad_branches.length == 1
        "The #{bad_branches.first} branch doesn’t exist."
      else
        "The #{bad_branches.join(' and ')} branches don’t exist."
      end
    elsif [:base_ref, :head_ref].any? { |attr| pr.errors[attr].include?(GitHub::Validations::Unicode3Validator::ERROR_MESSAGE) }
      "Branch names cannot contain unicode characters above 0xffff."
    else
      e.message
    end
  end

  # If there are this many timeline items or fewer, we'll render the timeline
  # inline when the user is viewing the Files tab. If there are more than this
  # many items, we'll leave the timeline out and load it as needed to try to
  # avoid timing out.
  MAXIMUM_TIMELINE_SIZE_FOR_INLINE_RENDER = 149

  def render_discussion_page?
    return @render_discussion_page if defined?(@render_discussion_page)
    @render_discussion_page =
      tab_specified?("discussion") || (!show_mobile_view? && @pull.timeline_children_for(current_user).count <= MAXIMUM_TIMELINE_SIZE_FOR_INLINE_RENDER)
  end
  helper_method :render_discussion_page?

  def render_files_page?
    return @render_files_page if defined?(@render_files_page)
    @render_files_page = tab_specified?("files") || (!show_mobile_view? && !@pull.corrupt? && !@pull.large_diff?)
  end
  helper_method :render_files_page?

  def pull_request_authorization_token
    current_user.signed_auth_token expires: 60.seconds.from_now,
                                   scope: pull_request_authorization_token_scope_key
  end

  def build_pull_request_diff_url
    route_options ||= {}

    if GitHub.prs_content_domain?
      route_options[:host] = GitHub.prs_content_host_name
    end

    if current_repository.private?
      route_options[:token] = pull_request_authorization_token
    end

    pull_request_raw_diff_url(route_options)
  end

  def build_pull_request_patch_url
    route_options ||= {}

    if GitHub.prs_content_domain?
      route_options[:host] = GitHub.prs_content_host_name
    end

    if current_repository.private?
      route_options[:token] = pull_request_authorization_token
    end

    pull_request_raw_patch_url(route_options)
  end

  def request_reflog_data(via)
    super(via).merge({ pr_author_login: @pull.safe_user.login })
  end

  def content_authorization_required
    authorize_content(:pull_request, repo: current_repository)
  end

  # Internal: Extract OID components from PR range.
  #
  #   /github/github/pull/123/files/abc123..def456
  #   /github/github/pull/123/files/def456
  #
  # pull  - Current PullRequest
  # range - String range parameter
  #
  # If a complete range is given, a pair of resolved String OIDs will be
  # returned. If only one end sha is given, nil and a resolved String OID
  # will be returned. Otherwise nil is returned if no range was matched.
  def parse_show_range_oid_components(pull, range)
    if m = range.to_s.match(/\A(?<sha1>[a-fA-F0-9]{7,40})\.\.(?<sha2>[a-fA-F0-9]{7,40}|HEAD)\z/)
      sha2 = m[:sha2] == "HEAD" ? pull.head_sha : m[:sha2]
      result = pull.repository.rpc.expand_shas([m[:sha1], sha2], "commit")
      sha1, sha2 = result[m[:sha1]], result[sha2]
      [sha1, sha2] if sha1 && sha2
    elsif m = range.to_s.match(/^(?<sha2>[a-fA-F0-9]{7,40})$/)
      result = pull.repository.rpc.expand_shas([m[:sha2]], "commit")
      sha2 = result[m[:sha2]]
      return [nil, sha2] if sha2
    end
  end

  def visible_timeline_items
    return [] unless render_discussion_page?
    super
  end

  def showing_full_timeline?
    return false unless render_discussion_page?
    super
  end

  def timeline_owner
    @pull
  end

  MobileCommitsQuery = parse_query <<-'GRAPHQL'
    query($ids: [ID!]!) {
      nodes(ids: $ids) {
        ... on Commit {
          id
          committer { date }
          ...Views::Mobile::Commits::GroupedCommit::Commit
        }
      }
    }
  GRAPHQL

  # XXX: Adhoc GraphQL loader for PullRequest.changedChanges connection.
  #
  # Load required GraphQL data for each Commit in PullRequest#changed_commits and
  # wrap it in a fake connection to be compatible with mobile/commits/list template.
  def load_mobile_pull_request_commits
    data = platform_execute(MobileCommitsQuery, variables: { "ids" => @pull.changed_commits.map(&:global_relay_id) })
    history = { "edges" => data.nodes.compact.map { |commit| { "node" => commit.to_h } }, "pageInfo" => {} }
  end
  helper_method :load_mobile_pull_request_commits
end
